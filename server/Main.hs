{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import           Control.Monad (unless, (>=>))
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger (runLogger')
import           Control.Monad.State (State)
import qualified Control.Monad.State as State
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Except (ExceptT(..), runExceptT)
import           Control.Monad.Trans.Reader (runReaderT)
import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as BL
import           Data.List (foldl')
import qualified Data.Map as M
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import           Data.Traversable (for)
import           GHC.Generics (Generic)
import qualified Language.PureScript as P
import qualified Language.PureScript.Bundle as Bundle
import qualified Language.PureScript.CodeGen.JS as J
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

server :: TL.Text -> [P.ExternsFile] -> P.Environment -> Int -> IO ()
server bundled externs initEnv port = do
  let compile :: Text -> IO (Either Error JS)
      compile input
        | T.length input > 20000 = return (Left (OtherError "Please limit your input to 20000 characters"))
        | otherwise = do
          let printErrors = P.prettyPrintMultipleErrors (P.defaultPPEOptions { P.ppeCodeColor = Nothing })
          case P.parseModuleFromFile (const "<file>") (undefined, input) of
            Left parseError ->
              return . Left . CompilerErrors . pure . P.toJSONError False P.Error . P.toPositionedError $ parseError
            Right (_, m) | P.getModuleName m == P.ModuleName [P.ProperName "Main"] -> do
              (resultMay, _) <- runLogger' . runExceptT . flip runReaderT P.defaultOptions $ do
                ((P.Module ss coms moduleName elaborated exps, env), nextVar) <- P.runSupplyT 0 $ do
                  [desugared] <- P.desugar externs [P.addDefaultImport (P.ModuleName [P.ProperName "Prim"]) m]
                  P.runCheck' (P.emptyCheckState initEnv) $ P.typeCheckModule desugared
                regrouped <- P.createBindingGroups moduleName . P.collapseBindingGroups $ elaborated
                let mod' = P.Module ss coms moduleName regrouped exps
                    corefn = CF.moduleToCoreFn env mod'
                    [renamed] = P.renameInModules [corefn]
                unless (null . CF.moduleForeign $ renamed) . throwError . P.errorMessage $ P.MissingFFIModule moduleName
                P.evalSupplyT nextVar $ P.prettyPrintJS <$> J.moduleToJs renamed Nothing
              case resultMay of
                Left errs -> (return . Left . CompilerErrors . P.toJSONErrors False P.Error) errs
                Right js -> (return . Right) js
            Right _ -> (return . Left. OtherError) "The name of the main module should be Main."

  scotty port $ do
    get "/" $
      Scotty.text "POST api.purescript.org/compile"
    get "/bundle" $ do
      Scotty.setHeader "Access-Control-Allow-Origin" "*"
      Scotty.setHeader "Content-Type" "text/javascript"
      Scotty.text bundled
    post "/compile" $ do
      code <- T.decodeUtf8 . BL.toStrict <$> body
      response <- lift $ compile code
      Scotty.setHeader "Access-Control-Allow-Origin" "*"
      case response of
        Left err ->
          Scotty.json $ A.object [ "error" .= err ]
        Right comp ->
          Scotty.json $ A.object [ "js" .= comp ]
    get "/search" $ do
      query <- param "q"
      case tryParseType query of
        Nothing -> Scotty.json $ A.object [ "error" .= ("Cannot parse type" :: Text) ]
        Just ty -> do
          let ty' = replaceTypeVariablesAndDesugar ty
          let results = TS.typeSearch (Just []) initEnv (P.emptyCheckState initEnv) ty'
          Scotty.json $ A.object [ "results" .= A.object [ P.showQualified P.runIdent k .= P.prettyPrintType v
                                                         | (k, v) <- take 20 (M.toList results)
                                                         ]
                                 ]

-- | (Consistently) replace unqualified type constructors and type variables with unknowns.
--
-- Also remove the @ParensInType@ Constructor (we need to deal with type operators later at some point).
replaceTypeVariablesAndDesugar :: P.Type -> P.Type
replaceTypeVariablesAndDesugar ty = State.evalState (P.everywhereOnTypesM go ty) (0, M.empty) where
  go = \case
    P.ParensInType ty -> pure ty
    P.TypeConstructor (P.Qualified Nothing tyCon) -> do
      (next, m) <- State.get
      case M.lookup (Left tyCon) m of
        Nothing -> do
          let ty = P.TUnknown next
          State.put (next + 1, M.insert (Left tyCon) ty m)
          pure ty
        Just ty -> pure ty
    P.TypeVar s -> do
      (next, m) <- State.get
      case M.lookup (Right s) m of
        Nothing -> do
          let ty = P.TUnknown next
          State.put (next + 1, M.insert (Right s) ty m)
          pure ty
        Just ty -> pure ty
    other -> pure other

tryParseType :: Text -> Maybe P.Type
tryParseType = hush (P.lex "") >=> hush (P.runTokenParser "" (P.parsePolyType <* Parsec.eof))
  where
    hush f = either (const Nothing) Just . f

bundle :: IO (Either Bundle.ErrorMessage String)
bundle = runExceptT $ do
  inputFiles <- liftIO (glob (".psci_modules" </> "node_modules" </> "*" </> "*.js"))
  input <- for inputFiles $ \filename -> do
    js <- liftIO (readUTF8File filename)
    mid <- Bundle.guessModuleIdentifier filename
    length js `seq` return (mid, js)
  Bundle.bundle input [] Nothing "PS"

main :: IO ()
main = do
  (portString : inputGlobs) <- getArgs
  let port = read portString
  inputFiles <- concat <$> traverse glob inputGlobs
  let onError f = either (Left . f) Right
  e <- runExceptT $ do
    modules <- ExceptT (fmap (onError Right) (I.loadAllModules inputFiles))
    (exts, env) <- ExceptT . fmap (onError Right) . I.runMake . I.make $ modules
    js <- ExceptT (fmap (onError Left) bundle)
    return (fromString js, exts, env)
  case e of
    Left err -> print err >> exitFailure
    Right (js, exts, env) -> server js exts env port
