-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Phil Freeman 2013-2015
-- License     :  MIT
--
-- Maintainer  :  paf31@cantab.net
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Main (
    main
) where

import qualified Data.Aeson as A
import           Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as BL
import           Data.List (foldl')
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Control.Monad (unless)
import           Control.Monad.Logger (runLogger')
import           Control.Monad.Trans (lift)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Reader (runReaderT)

import qualified Language.PureScript as P
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.CodeGen.JS as J

import           System.Environment (getArgs)
import           System.FilePath ((</>))
import qualified System.IO as IO

import           Web.Scotty
import qualified Web.Scotty as Scotty

type JS = String

readExterns :: FilePath -> IO P.ExternsFile
readExterns path = do
  h <- IO.openFile path IO.ReadMode
  IO.hSetEncoding h IO.utf8
  either (error . (("Error reading externs file " ++ path ++ ": ") ++)) id
    . A.eitherDecode
    . fromString
    <$> IO.hGetContents h

server :: [P.ExternsFile] -> Int -> IO ()
server externs port = do
  let initEnv = foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment externs

  let compile :: String -> IO (Either String JS)
      compile input
        | length input > 20000 = return $ Left "Please limit your input to 20000 characters"
        | otherwise = do
          let printErrors = P.prettyPrintMultipleErrors P.defaultPPEOptions
          case P.parseModuleFromFile (const "<file>") (undefined, input) of
            Left parseError ->
              return . Left . printErrors . P.MultipleErrors . return . P.toPositionedError $ parseError
            Right (_, m) | P.getModuleName m == P.ModuleName [P.ProperName "Main"] -> do
              (resultMay, _) <- runLogger' . runExceptT . flip runReaderT P.defaultOptions $ do
                ((P.Module ss coms moduleName elaborated exps, env), nextVar) <- P.runSupplyT 0 $ do
                  [desugared] <- P.desugar externs [P.addDefaultImport (P.ModuleName [P.ProperName "Prim"]) m]
                  P.runCheck' initEnv $ P.typeCheckModule desugared
                regrouped <- P.createBindingGroups moduleName . P.collapseBindingGroups $ elaborated
                let mod' = P.Module ss coms moduleName regrouped exps
                    corefn = CF.moduleToCoreFn env mod'
                    [renamed] = P.renameInModules [corefn]
                unless (null . CF.moduleForeign $ renamed) . throwError . P.errorMessage $ P.MissingFFIModule moduleName
                P.evalSupplyT nextVar $ P.prettyPrintJS <$> J.moduleToJs renamed Nothing
              case resultMay of
                Left errs -> return . Left . printErrors $ errs
                Right js -> return (Right js)
            Right _ -> return $ Left "The name of the main module should be Main."

  scotty port $ do
    get "/" $
      Scotty.text "POST api.purescript.org/compile"
    post "/compile" $ do
      code <- T.unpack . T.decodeUtf8 . BL.toStrict <$> body
      response <- lift $ compile code
      case response of
        Left err ->
          Scotty.json $ A.object [ "error" .= err ]
        Right comp ->
          Scotty.json $ A.object [ "js" .= comp ]

main :: IO ()
main = do
  [externsPath, confFile, port] <- getArgs
  externsFiles <- filter (not . null) . lines <$> readFile confFile
  externs <- mapM (readExterns . (externsPath </>) . (++ ".json")) externsFiles
  server externs (read port)
