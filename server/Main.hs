{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import           Control.Monad (unless, (>=>), foldM)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runLogger')
import           Control.Monad.State (State)
import qualified Control.Monad.State as State
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Reader (runReaderT)
import           Control.Monad.Writer.Strict (runWriterT)
import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import           Data.Bifunctor (first, second)
import qualified Data.ByteString.Lazy as BL
import           Data.Function (on)
import           Data.List (foldl', nubBy)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as M
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import           Data.Traversable (for)
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
import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           System.FilePath ((</>))
import           System.FilePath.Glob (glob)
import qualified System.IO as IO
import           System.IO.UTF8 (readUTF8File)
import qualified Text.Parsec.Combinator as Parsec
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
  let compile :: Text -> IO (Either Error ([P.JSONError], JS))
      compile input
        | T.length input > 20000 = return (Left (OtherError "Please limit your input to 20000 characters"))
        | otherwise = do
          let printErrors = P.prettyPrintMultipleErrors (P.defaultPPEOptions { P.ppeCodeColor = Nothing })
          case CST.parseModuleFromFile "<file>" input >>= CST.resFull of
            Left parseError ->
              return . Left . CompilerErrors . P.toJSONErrors False P.Error $ CST.toMultipleErrors "<file>" parseError
            Right m | P.getModuleName m == P.ModuleName "Main" -> do
              (resultMay, ws) <- runLogger' . runExceptT . flip runReaderT P.defaultOptions $ do
                ((P.Module ss coms moduleName elaborated exps, env), nextVar) <- P.runSupplyT 0 $ do
                  desugared <- P.desugar initNamesEnv externs [P.importPrim m] >>= \case
                    [d] -> pure d
                    _ -> error "desugaring did not produce one module"
                  P.runCheck' (P.emptyCheckState initEnv) $ P.typeCheckModule desugared
                regrouped <- P.createBindingGroups moduleName . P.collapseBindingGroups $ elaborated
                let mod' = P.Module ss coms moduleName regrouped exps
                    corefn = CF.moduleToCoreFn env mod'
                    [renamed] = P.renameInModules [corefn]
                unless (null . CF.moduleForeign $ renamed) . throwError . P.errorMessage $ P.MissingFFIModule moduleName
                P.evalSupplyT nextVar $ P.prettyPrintJS <$> J.moduleToJs renamed Nothing
              case resultMay of
                Left errs -> (return . Left . CompilerErrors . P.toJSONErrors False P.Error) errs
                Right js -> (return . Right) (P.toJSONErrors False P.Error ws, js)
            Right _ ->
              (return . Left . OtherError) "The name of the main module should be Main."

  scotty port $ do
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
                let strictMatches = search (replaceTypeVariablesAndDesugar (\nm s -> P.Skolem P.NullSourceAnn nm s (P.SkolemScope 0)) elab)
                    flexMatches = search (replaceTypeVariablesAndDesugar (const (P.TUnknown P.NullSourceAnn)) elab)
                take 50 (strictMatches ++ flexMatches)
          Scotty.json $ A.object [ "results" .= [ P.showQualified id k
                                                | (k, _) <- take 50 results
                                                ]
                                 ]

lookupAllConstructors :: P.Environment -> P.SourceType -> [P.SourceType]
lookupAllConstructors env = P.everywhereOnTypesM $ \case
    P.TypeConstructor ann (P.Qualified Nothing tyCon) -> P.TypeConstructor ann <$> lookupConstructor env tyCon
    other -> pure other
  where
    lookupConstructor :: P.Environment -> P.ProperName 'P.TypeName -> [P.Qualified (P.ProperName 'P.TypeName)]
    lookupConstructor env nm =
      [ q
      | (q@(P.Qualified (Just mn) thisNm), _) <- M.toList (P.types env)
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
       first (CST.prettyPrintError . NE.head)
        . CST.runTokenParser (p <* CSTM.token CST.TokEof)
        . CST.lexTopLevel

main :: IO ()
main = do
  (portString : inputGlobs) <- getArgs
  let port = read portString
  inputFiles <- concat <$> traverse glob inputGlobs
  let onError f = either (Left . f) Right
  e <- runExceptT $ do
    modules <- ExceptT $ I.loadAllModules inputFiles
    (exts, env) <- ExceptT . I.runMake . I.make $ map (second CST.pureResult) modules
    namesEnv <- fmap fst . runWriterT $ foldM P.externsEnv P.primEnv exts
    pure (exts, namesEnv, env)
  case e of
    Left err -> print err >> exitFailure
    Right (exts, namesEnv, env) -> server exts namesEnv env port
