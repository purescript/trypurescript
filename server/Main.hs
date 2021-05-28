{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}

module Main (main) where

import           Control.Monad (unless, foldM)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runLogger')
import qualified Control.Monad.State as State
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Writer.Strict (runWriterT)
import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import           Data.Bifunctor (first, second, bimap)
import qualified Data.ByteString.Lazy as BL
import           Data.Default (def)
import           Data.Function (on)
import qualified Data.IORef as IORef
import           Data.List (nubBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime)
import           GHC.Generics (Generic)
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.CST.Monad as CSTM
import qualified Language.PureScript.CodeGen.JS as J
import qualified Language.PureScript.CodeGen.JS.Printer as P
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.Docs.Types as Docs
import qualified Language.PureScript.Errors.JSON as P
import qualified Language.PureScript.Interactive as I
import qualified Language.PureScript.Make as Make
import qualified Language.PureScript.Make.Cache as Cache
import qualified Language.PureScript.TypeChecker.TypeSearch as TS
import qualified Network.Wai.Handler.Warp as Warp
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.FilePath.Glob (glob)
import           Web.Scotty
import qualified Web.Scotty as Scotty

type JS = Text

data Error
  = CompilerErrors [P.JSONError]
  | OtherError Text
  deriving Generic

instance A.ToJSON Error

toCompilerErrors :: NE.NonEmpty CST.ParserError -> Error
toCompilerErrors = CompilerErrors . toJsonErrors . CST.toMultipleErrors "<file>"

toJsonErrors :: P.MultipleErrors -> [P.JSONError]
toJsonErrors = P.toJSONErrors False P.Error

-- As of PureScript 0.14 we only need the `codegen` part of `MakeActions` to run
-- Try PureScript, because we already know all dependencies are compiled, we're
-- only building one module, we don't allow FFI declarations, and we want to
-- avoid writing to the file system as much as possible.
buildMakeActions :: IORef.IORef (Maybe JS) -> Make.MakeActions Make.Make
buildMakeActions codegenRef =
  Make.MakeActions
    getInputTimestampsAndHashes
    getOutputTimestamp
    readExterns
    codegen
    ffiCodegen
    progress
    readCacheDb
    writeCacheDb
    outputPrimDocs
  where
  getInputTimestampsAndHashes :: P.ModuleName -> Make.Make (Either Make.RebuildPolicy (M.Map FilePath (UTCTime, Make.Make Cache.ContentHash)))
  getInputTimestampsAndHashes _ = pure $ Right M.empty

  getOutputTimestamp :: P.ModuleName -> Make.Make (Maybe UTCTime)
  getOutputTimestamp _ = pure Nothing

  readExterns :: P.ModuleName -> Make.Make (FilePath, Maybe P.ExternsFile)
  readExterns _ = pure ("<file>", Nothing)

  codegen :: CF.Module CF.Ann -> Docs.Module -> P.ExternsFile -> P.SupplyT Make.Make ()
  codegen m _ _ = do
    rawJs <- J.moduleToJs m Nothing
    lift $ liftIO $ IORef.writeIORef codegenRef $ Just $ P.prettyPrintJS rawJs

  -- If we ever support FFI implementations in Try PureScript then we will need
  -- to implement this function. However, we do not plan to support this feature.
  ffiCodegen :: CF.Module CF.Ann -> Make.Make ()
  ffiCodegen _ = pure ()

  progress :: Make.ProgressMessage -> Make.Make ()
  progress _ = pure ()

  readCacheDb :: Make.Make Cache.CacheDb
  readCacheDb = pure M.empty

  writeCacheDb :: Cache.CacheDb -> Make.Make ()
  writeCacheDb _ = pure ()

  outputPrimDocs :: Make.Make ()
  outputPrimDocs = pure ()

server :: [P.ExternsFile] -> P.Environment -> Int -> IO ()
server externs initEnv port = do
  codegenRef <- IORef.newIORef Nothing
  let makeActions = buildMakeActions codegenRef
  let compile :: Text -> IO (Either Error ([P.JSONError], JS))
      compile input
        | T.length input > 20000 = return $ Left $ OtherError "Please limit your input to 20000 characters"
        | otherwise = do
          case CST.parseModuleFromFile "<file>" input of
            Left parserErrors ->
              return $ Left $ toCompilerErrors parserErrors

            Right partialResult -> case CST.resFull partialResult of
              (_, Left parserErrors) ->
                return $ Left $ toCompilerErrors parserErrors

              (parserWarnings, Right m) | P.getModuleName m == P.ModuleName "Main" -> do
                (makeResult, warnings) <- Make.runMake P.defaultOptions $ Make.rebuildModule makeActions externs m
                codegenResult <- IORef.readIORef codegenRef
                return $ case makeResult of
                  Left errors ->
                    Left $ CompilerErrors $ toJsonErrors errors
                  Right _ | Just js <- codegenResult -> do
                    let ws = warnings <> CST.toMultipleWarnings "<file>" parserWarnings
                    Right (toJsonErrors ws, js)
                  Right _ ->
                    Left $ OtherError "Failed to read the results of codegen."

              (_, Right _) ->
                return $ Left $ OtherError "The name of the main module should be Main."

  scottyOpts (getOpts port) $ do
    get "/" $
      Scotty.text "POST api.purescript.org/compile"

    post "/compile" $ do
      code <- T.decodeUtf8 . BL.toStrict <$> body
      response <- lift $ compile code
      Scotty.setHeader "Access-Control-Allow-Origin" "*"
      case response of
        Left err ->
          Scotty.json $ A.object [ "error" .= err ]
        Right (warnings, comp) ->
          Scotty.json $ A.object [ "js" .= comp, "warnings" .= warnings ]

    get "/search" $ do
      query <- param "q"
      Scotty.setHeader "Access-Control-Allow-Origin" "*"
      Scotty.setHeader "Content-Type" "application/json"
      case tryParseType query of
        Nothing -> Scotty.json $ A.object [ "error" .= ("Cannot parse type" :: Text) ]
        Just ty -> do
          let elabs = lookupAllConstructors initEnv ty
              search = fst . TS.typeSearch (Just []) initEnv (P.emptyCheckState initEnv)
              results = nubBy ((==) `on` fst) $ do
                elab <- elabs
                let mkSkolemType nm s = P.Skolem P.NullSourceAnn nm Nothing s (P.SkolemScope 0)
                    strictMatches = search (replaceTypeVariablesAndDesugar mkSkolemType elab)
                    flexMatches = search (replaceTypeVariablesAndDesugar (const (P.TUnknown P.NullSourceAnn)) elab)
                take 50 (strictMatches ++ flexMatches)
          Scotty.json $ A.object [ "results" .= [ P.showQualified id k
                                                | (k, _) <- take 50 results
                                                ]
                                 ]

getOpts :: Int -> Scotty.Options
getOpts port = def
  { settings =
      Warp.setHost "127.0.0.1"
      $ Warp.setPort port
      $ Warp.defaultSettings
  }

lookupAllConstructors :: P.Environment -> P.SourceType -> [P.SourceType]
lookupAllConstructors env = P.everywhereOnTypesM $ \case
    P.TypeConstructor ann (P.Qualified Nothing tyCon) -> P.TypeConstructor ann <$> lookupConstructor env tyCon
    other -> pure other
  where
    lookupConstructor :: P.Environment -> P.ProperName 'P.TypeName -> [P.Qualified (P.ProperName 'P.TypeName)]
    lookupConstructor env nm =
      [ q
      | (q@(P.Qualified (Just _) thisNm), _) <- M.toList (P.types env)
      , thisNm == nm
      ]

-- | (Consistently) replace unqualified type constructors and type variables with unknowns.
--
-- Also remove the @ParensInType@ Constructor (we need to deal with type operators later at some point).
replaceTypeVariablesAndDesugar :: (Text -> Int -> P.SourceType) -> P.SourceType -> P.SourceType
replaceTypeVariablesAndDesugar f ty = State.evalState (P.everywhereOnTypesM go ty) (0, M.empty) where
  go = \case
    P.ParensInType _ ty -> pure ty
    P.TypeVar _ s -> do
      (next, m) <- State.get
      case M.lookup s m of
        Nothing -> do
          let ty = f s next
          State.put (next + 1, M.insert s ty m)
          pure ty
        Just ty -> pure ty
    other -> pure other

tryParseType :: Text -> Maybe P.SourceType
tryParseType = hush . fmap (CST.convertType "<file>") . runParser CST.parseTypeP
  where
    hush = either (const Nothing) Just

    runParser :: CST.Parser a -> Text -> Either String a
    runParser p =
      bimap (CST.prettyPrintError . NE.head) snd
        . CST.runTokenParser (p <* CSTM.token CST.TokEof)
        . CST.lexTopLevel

main :: IO ()
main = do
  (portString : inputGlobs) <- getArgs
  let port = read portString
  inputFiles <- concat <$> traverse glob inputGlobs
  e <- runExceptT $ do
    modules <- ExceptT $ I.loadAllModules inputFiles
    ExceptT . I.runMake . I.make $ map (second CST.pureResult) modules
  case e of
    Left err -> print err >> exitFailure
    Right (exts, env) -> server exts env port
