{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Monad (unless, foldM)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Logger (runLogger')
import qualified Control.Monad.State as State
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Writer.Strict (runWriterT)
import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import           Data.Bifunctor (first, second)
import qualified Data.ByteString.Lazy as BL
import           Data.Default (def)
import           Data.Function (on)
import           Data.List (nubBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           GHC.Generics (Generic)
import qualified Language.PureScript as P
import qualified Language.PureScript.CST as CST
import qualified Language.PureScript.CST.Monad as CSTM
import qualified Language.PureScript.CodeGen.JS as J
import qualified Language.PureScript.CodeGen.JS.Printer as P
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.Errors.JSON as P
import qualified Language.PureScript.Interactive as I
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

server :: [P.ExternsFile] -> P.Env -> P.Environment -> Int -> IO ()
server externs initNamesEnv initEnv port = do
              -- module contents           warnings       compiled result
  let compile :: Text -> IO (Either Error ([P.JSONError], JS))
      compile input
        | T.length input > 20000 = return (Left (OtherError "Please limit your input to 20000 characters"))
        | otherwise = do
          case CST.parseModuleFromFile "<file>" input of -- >>= CST.resFull of
            Left parseError ->
              return . Left . CompilerErrors . P.toJSONErrors False P.Error $ CST.toMultipleErrors "<file>" parseError

            Right partialResult -> case CST.resFull partialResult of
              (warnings, Left parseError) ->
                -- TODO: Maybe return errors and warnings if possible; otherwise, fall back to
                -- only returning parse error.
                -- _ -- wildcard
                return . Left . CompilerErrors . P.toJSONErrors False P.Error $ CST.toMultipleErrors "<file>" parseError

              (warnings, Right m) | P.getModuleName m == P.ModuleName "Main" -> do
                (resultMay, ws) <- runLogger' . runExceptT . flip runReaderT P.defaultOptions $ do
                  ((P.Module ss coms moduleName elaborated exps, env), nextVar) <- P.runSupplyT 0 $ do
                    (desugared, (namesEnv, _)) <- State.runStateT (P.desugar externs (P.importPrim m)) (initNamesEnv, mempty)
                    let typecheck = P.typeCheckModule (fmap (\(_, _, exports) -> exports) namesEnv) desugared
                    P.runCheck' (P.emptyCheckState initEnv) typecheck
                  regrouped <- P.createBindingGroups moduleName . P.collapseBindingGroups $ elaborated
                  let mod' = P.Module ss coms moduleName regrouped exps
                      corefn = CF.moduleToCoreFn env mod'
                      [renamed] = P.renameInModules [corefn]
                  unless (null . CF.moduleForeign $ renamed) . throwError . P.errorMessage $ P.MissingFFIModule moduleName
                  P.evalSupplyT nextVar $ P.prettyPrintJS <$> J.moduleToJs renamed Nothing
                case resultMay of
                  Left errs -> (return . Left . CompilerErrors . P.toJSONErrors False P.Error) errs
                  Right js -> (return . Right) (P.toJSONErrors False P.Error ws, js)

              (_, Right _) ->
                (return . Left . OtherError) "The name of the main module should be Main."

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
                let strictMatches = search (replaceTypeVariablesAndDesugar (\nm s -> P.Skolem P.NullSourceAnn nm Nothing s (P.SkolemScope 0)) elab)
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

    -- TODO: Should we preserve and use the [CST.ParserWarning] result here
    -- instead of throwing it away?
    runParser :: CST.Parser a -> Text -> Either String a
    runParser p =
      fmap snd
        . first (CST.prettyPrintError . NE.head)
        . CST.runTokenParser (p <* CSTM.token CST.TokEof)
        . CST.lexTopLevel

main :: IO ()
main = do
  (portString : inputGlobs) <- getArgs
  let port = read portString
  inputFiles <- concat <$> traverse glob inputGlobs
  e <- runExceptT $ do
    modules <- ExceptT $ I.loadAllModules inputFiles
    (exts, env) <- ExceptT . I.runMake . I.make $ map (second CST.pureResult) modules
    namesEnv <- fmap fst . runWriterT $ foldM P.externsEnv P.primEnv exts
    pure (exts, namesEnv, env)
  case e of
    Left err -> print err >> exitFailure
    Right (exts, namesEnv, env) -> server exts namesEnv env port
