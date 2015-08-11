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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Main (
    main
) where

import qualified Language.PureScript as P
import qualified Language.PureScript.CodeGen.JS as J
import qualified Language.PureScript.CoreFn as CF
import qualified Language.PureScript.Bundle as B

import Data.Version (showVersion)
import Data.Monoid
import Data.String
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import Data.FileEmbed
import Data.Time.Clock (UTCTime())
import Data.Foldable (traverse_)

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy.Char8 as BLC8

import qualified Data.Aeson as A
import Data.Aeson ((.=))

import qualified Data.Map as M

import Control.Applicative
import Control.Monad (when, forM_)
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.Error.Class (MonadError(..))
import Control.Monad.Trans.Except
import Control.Monad.Reader
import Control.Monad.Writer

import Network.HTTP.Types (status500)

import Web.Scotty
import qualified Web.Scotty as Scotty

import Text.Blaze.Html
import Text.Blaze.Internal
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import qualified Paths_trypurescript as Paths

import System.Environment (getArgs)

newtype Compiled = Compiled { runCompiled :: String }

newtype Response = Response { runResponse :: Either String Compiled }

type FS = M.Map B.ModuleIdentifier String

newtype Try a = Try { unTry :: ReaderT P.Options (WriterT P.MultipleErrors (WriterT FS (Either P.MultipleErrors))) a }
  deriving (Functor, Applicative, Monad, MonadError P.MultipleErrors, MonadWriter P.MultipleErrors, MonadReader P.Options)

runTry :: Try a -> Either P.MultipleErrors (a, FS)
runTry = runWriterT . fmap fst . runWriterT . flip runReaderT P.defaultOptions . unTry

writeTextFileTry :: B.ModuleIdentifier -> String -> Try ()
writeTextFileTry mid txt = Try . lift . lift . tell $ M.singleton mid txt

makeActions :: M.Map P.ModuleName (FilePath, P.ForeignJS) -> P.MakeActions Try
makeActions foreigns = P.MakeActions getInputTimestamp getOutputTimestamp readExterns codegen progress
  where
  getInputTimestamp :: P.ModuleName -> Try (Either P.RebuildPolicy (Maybe UTCTime))
  getInputTimestamp _ = return (Left P.RebuildAlways)
  
  getOutputTimestamp :: P.ModuleName -> Try (Maybe UTCTime)
  getOutputTimestamp mn = return (Just (error "getOutputTimestamp: read timestamp"))

  readExterns :: P.ModuleName -> Try (FilePath, String)
  readExterns _ = error "readExterns: not supported"
  
  codegen :: CF.Module CF.Ann -> P.Environment -> P.SupplyVar -> P.Externs -> Try ()
  codegen m _ nextVar exts = do
    let mn = P.runModuleName (CF.moduleName m)
    foreignInclude <- case (CF.moduleName m `M.lookup` foreigns, CF.moduleForeign m) of
      (Just path, fs) | not (null fs) -> 
        return $ Just $ J.JSApp (J.JSVar "require") [J.JSStringLiteral "./foreign"]
      _ -> 
        return Nothing
    pjs <- P.evalSupplyT nextVar $ P.prettyPrintJS <$> J.moduleToJs m foreignInclude
    writeTextFileTry (B.ModuleIdentifier mn B.Regular) pjs
    traverse_ (writeTextFileTry (B.ModuleIdentifier mn B.Foreign) . snd) (CF.moduleName m `M.lookup` foreigns)
      
  progress :: String -> Try ()
  progress _ = return ()
  
compile :: [P.Module] -> M.Map P.ModuleName (FilePath, P.ForeignJS) -> String -> IO Response
compile _ _ input | length input > 20000 = return $ Response $ Left "Please limit your input to 20000 characters"
compile prelude foreigns input = do
  case either Left (Right . map snd) $ P.parseModulesFromFiles (const "<file>") [(undefined, input)] of
    Left parseError -> do
      return $ Response $ Left $ P.prettyPrintMultipleErrors False parseError
    Right modules ->
      case runTry (P.make (makeActions foreigns) (prelude ++ modules)) of
        Left err ->
          return $ Response $ Left (P.prettyPrintMultipleErrors False err)
        Right (_, fs) ->
          case B.bundle (M.toList fs) [B.ModuleIdentifier "Main" B.Regular] (Just "Main") "TryPS" of
            Left err -> return $ Response $ Left (unlines (B.printErrorMessage err))
            Right js -> return $ Response $ Right $ Compiled js

str :: String -> String
str = id

mono :: H.Html -> H.Html
mono h = h ! A.class_ "mono"

css :: String
css = BC8.unpack $(embedFile "assets/style.css")

gaq :: String
gaq = BC8.unpack $(embedFile "assets/gaq.js")

scripts :: String
scripts = BC8.unpack $(embedFile "assets/scripts.js")

defaultCode :: String
defaultCode = BC8.unpack $(embedFile "examples/default.purs")

preludePurs :: [String]
preludePurs = 
  [ BC8.unpack $(embedFile "prelude/Prelude.purs")
  , BC8.unpack $(embedFile "prelude/Control/Monad/Eff.purs")
  , BC8.unpack $(embedFile "prelude/Control/Monad/Eff/Console.purs")
  ]

preludeJs :: [String]
preludeJs = 
  [ BC8.unpack $(embedFile "prelude/Prelude.js")
  , BC8.unpack $(embedFile "prelude/Control/Monad/Eff.js")
  , BC8.unpack $(embedFile "prelude/Control/Monad/Eff/Console.js")
  ]

examples :: [(String, (String, String))]
examples =
  [ ("adt",         ("Algebraic Data Types",  BC8.unpack $(embedFile "examples/adt.purs")))
  , ("ops",         ("Operators",             BC8.unpack $(embedFile "examples/operators.purs")))
  , ("rows",        ("Row Polymorphism",      BC8.unpack $(embedFile "examples/rows.purs")))
  , ("recursion",   ("Recursion",             BC8.unpack $(embedFile "examples/recursion.purs")))
  , ("do",          ("Do Notation",           BC8.unpack $(embedFile "examples/do.purs")))
  , ("tco",         ("Tail-Call Elimination", BC8.unpack $(embedFile "examples/tco.purs")))
  , ("typeclasses", ("Type Classes",          BC8.unpack $(embedFile "examples/typeclasses.purs")))
  ]

page :: String -> ActionM ()
page input = html $ renderHtml $ do
  H.docType
  H.html $ do
    H.head $ do
      H.title $ H.toHtml $ str "Try PureScript!"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "http://fonts.googleapis.com/css?family=Roboto:300,600"
      H.style $ H.toHtml $ str css
      H.script ! A.type_ "text/javascript" $ preEscapedToHtml gaq
      H.script ! A.type_ "text/javascript" ! A.src "//cdnjs.cloudflare.com/ajax/libs/jquery/1.10.2/jquery.js" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "//cdnjs.cloudflare.com/ajax/libs/ace/1.1.01/ace.js" ! A.charset "utf-8" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "//cdnjs.cloudflare.com/ajax/libs/ace/1.1.01/mode-haskell.js" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "//cdnjs.cloudflare.com/ajax/libs/ace/1.1.01/theme-dawn.js" $ mempty
    H.body $ do
      H.div ! A.class_ "wrapper" $ do
         H.div ! A.class_ "header" $ do
             H.h1 $ H.toHtml $ str "Try PureScript!"
         H.div ! A.class_ "body" $ do
             H.p $ H.toHtml $ str "Type PureScript code below and press 'Compile', or select one of the examples below:"
      
             H.h2 $ H.toHtml $ str "Examples"
             H.ul $ do
               forM_ examples $ \(name, (title, _)) ->
                 H.li $ H.a ! A.href (fromString $ "/example/" ++ name) $ H.toHtml title
      
             H.h2 $ H.toHtml $ str "PureScript Code"
             H.div ! A.id "code" $ mempty
             H.textarea ! A.name "code" ! A.id "textarea" ! A.style "display: none;" $ H.toHtml $ str input
             H.p $ H.button ! A.id "compile" $ H.toHtml $ str "Compile and Run"
             H.script ! A.type_ "text/javascript" $ preEscapedToHtml scripts
             H.div ! A.id "results" $ mempty

server :: Int -> IO ()
server port = do
  let preludeModules = either (error . show) (map snd) $ P.parseModulesFromFiles (const "<prelude>") (map (undefined, ) preludePurs)
  Right (foreigns, _) <- runExceptT $ runWriterT $ P.parseForeignModulesFromFiles (map (error "foreign filename read", ) preludeJs)
  scotty port $ do
    get "/" $ do
      page defaultCode
    get "/example/:name" $ do
      name <- param "name"
      case lookup name examples of
        Nothing -> raise "No such example"
        Just (_, code) -> do
          page code
    post "/compile/text" $ do
      code <- BLC8.unpack <$> body
      response <- lift $ compile preludeModules foreigns code
      case runResponse response of
        Left err -> do
          Scotty.json $ A.object [ "error" .= err ]
        Right comp -> 
          Scotty.json $ A.object [ "js" .= runCompiled comp ]

main :: IO ()
main = do
  [port] <- getArgs
  server (read port)
