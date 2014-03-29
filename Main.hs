-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  (c) Phil Freeman 2013
-- License     :  MIT
--
-- Maintainer  :  paf31@cantab.net
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}

module Main (
    main
) where

import Web.Scotty
import qualified Language.PureScript as P

import Data.Version (showVersion)

import Data.Monoid
import Data.String
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
import System.Console.CmdTheLine
import Control.Applicative
import Control.Monad (when, forM_)
import Control.Monad.Trans
import qualified Data.Map as M
import Text.Blaze.Html
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified System.IO.UTF8 as U

import qualified Paths_trypurescript as Paths

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU

import Data.FileEmbed

prelude :: String
prelude = BU.toString $(embedFile "prelude/prelude.purs")

data Compiled = Compiled { js      :: String
                         , externs :: String
                         }

data Response = Response (Either String Compiled)

options :: P.Options
options = P.defaultOptions { P.optionsModules = ["Main"] }

compile :: [P.Module] -> String -> IO Response
compile _ input | length input > 5000 = return $ Response $ Left "Please limit your input to 5000 characters"
compile prelude input = do
  case P.runIndentParser "" P.parseModules input of
    Left parseError -> do
      return $ Response $ Left $ show parseError
    Right modules -> do
      case P.compile options (prelude ++ modules) of
        Left error ->
          return $ Response $ Left error
        Right (js, externs, _) ->
          return $ Response $ Right $ Compiled js externs

str :: String -> String
str = id

mono :: H.Html -> H.Html
mono h = h ! A.class_ "mono"

examplesJs :: String
examplesJs = BU.toString $(embedFile "assets/examples.js")

css :: String
css = BU.toString $(embedFile "assets/style.css")

gaq :: String
gaq = BU.toString $(embedFile "assets/gaq.js")

ace :: String
ace = BU.toString $(embedFile "assets/ace.js")

defaultCode :: String
defaultCode = BU.toString $(embedFile "examples/default.purs")

examples :: [(String, (String, String))]
examples =
  [ ("adt",         ("Algebraic Data Types",  BU.toString $(embedFile "examples/adt.purs")))
  , ("ops",         ("Operators",             BU.toString $(embedFile "examples/operators.purs")))
  , ("arrays",      ("Arrays",                BU.toString $(embedFile "examples/arrays.purs")))
  , ("rows",        ("Row Polymorphism",      BU.toString $(embedFile "examples/rows.purs")))
  , ("ffi",         ("FFI",                   BU.toString $(embedFile "examples/ffi.purs")))
  , ("mutable",     ("Mutable Variables",     BU.toString $(embedFile "examples/mutable.purs")))
  , ("modules",     ("Modules",               BU.toString $(embedFile "examples/modules.purs")))
  , ("rank2",       ("Rank N Types",          BU.toString $(embedFile "examples/rankn.purs")))
  , ("recursion",   ("Recursion",             BU.toString $(embedFile "examples/recursion.purs")))
  , ("do",          ("Do Notation",           BU.toString $(embedFile "examples/do.purs")))
  , ("tco",         ("Tail-Call Elimination", BU.toString $(embedFile "examples/tco.purs")))
  , ("typeclasses", ("Type Classes",          BU.toString $(embedFile "examples/typeclasses.purs")))
  ]

page :: Maybe String -> Maybe String -> Maybe Response -> ActionM ()
page ex input res = html $ renderHtml $ do
  H.docType
  H.html $ do
    H.head $ do
      H.title $ H.toHtml $ str "Try PureScript!"
      H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1.0"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "http://fonts.googleapis.com/css?family=Lato:300,400,700"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "http://fonts.googleapis.com/css?family=Ubuntu+Mono"
      H.style $ H.toHtml $ str css
      H.script ! A.type_ "text/javascript" $ preEscapedToHtml gaq
      H.script ! A.type_ "text/javascript" ! A.src "//cdnjs.cloudflare.com/ajax/libs/jquery/1.10.2/jquery.js" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "//cdnjs.cloudflare.com/ajax/libs/ace/1.1.01/ace.js" ! A.charset "utf-8" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "//cdnjs.cloudflare.com/ajax/libs/ace/1.1.01/mode-haskell.js" $ mempty
      H.script ! A.type_ "text/javascript" ! A.src "//cdnjs.cloudflare.com/ajax/libs/ace/1.1.01/theme-dawn.js" $ mempty
    H.body $ do
        H.div ! A.class_ "header" $ do
           H.div ! A.class_ "center" $ do
	       H.h1 $ H.toHtml $ str "Try PureScript!"
	       H.p $ H.toHtml $ str "Type PureScript code below and press 'Compile' to view the compiled Javascript."
	       H.p $ mconcat [ H.a ! A.href "http://functorial.com/purescript" $ H.toHtml $ str "Documentation"
		             , H.toHtml $ str ", "
		             , H.a ! A.href "http://github.com/paf31/purescript" $ H.toHtml $ str "Compiler Source"
		             , H.toHtml $ str ", "
		             , H.a ! A.href "http://github.com/paf31/trypurescript" $ H.toHtml $ str "Try PureScript Source" ]
           H.div ! A.class_ "splitter" $ mempty
        H.div ! A.class_ "main" $ do
           H.div ! A.class_ "center" $ do
	       let (success, text) = responseToJs res
	       H.div $ do
	         H.select ! A.style "float: right;" ! A.id "examples" $ do
	           H.option ! A.value "" $ "Examples"
	           H.option ! A.value "" $ ""
	           forM_ examples $ \(name, (title, _)) -> case () of
	             _ | ex == Just name -> H.option ! A.value (fromString name) ! A.selected "selected" $ H.toHtml title
	             _ ->  H.option ! A.value (fromString name) $ H.toHtml title
		   H.script ! A.type_ "text/javascript" $ preEscapedToHtml examplesJs
	       H.div ! A.style "clear: right;" $ mempty
	       H.div ! A.style "position: relative; "$ do
	         H.div ! A.style "position: absolute; width: 50%;" $ do
                   H.h2 $ H.toHtml $ str "PureScript Code"
	           H.form ! A.action "/compile" ! A.method "POST" $ do
		     H.div ! A.id "code" $ mempty
		     H.textarea ! A.name "code" ! A.id "textarea" ! A.style "display: none;" $ maybe mempty (H.toHtml . str) input
		     H.div $ H.button ! A.type_ "submit" $ H.toHtml $ str "Compile"
	         H.div ! A.style "position: absolute; width: 50%; left: 50%;" $ do
		   H.h2 $ H.toHtml $ str "Generated Javascript"
	           H.div ! A.id "js" $ H.toHtml . str $ text
	       H.script ! A.type_ "text/javascript" $ preEscapedToHtml $ str $ "var compiledSuccessfully = " ++ if success then "true;" else "false;"
	       H.script ! A.type_ "text/javascript" $ preEscapedToHtml ace

responseToJs :: Maybe Response -> (Bool, String)
responseToJs Nothing = (False, "")
responseToJs (Just (Response (Left err))) = (False, err)
responseToJs (Just (Response (Right (Compiled "" "")))) = (False, "Please enter some input")
responseToJs (Just (Response (Right (Compiled js _)))) = (True, js)

server :: Int -> IO ()
server port = do
  let preludeModules = either (error . show) id $ P.runIndentParser "" P.parseModules prelude
  scotty port $ do
    get "/" $ do
      page Nothing (Just defaultCode) Nothing
    get "/example/:name" $ do
      name <- param "name"
      case lookup name examples of
        Nothing -> raise "No such example"
        Just (_, code) -> do
          response <- lift $ compile preludeModules code
          page (Just name) (Just code) (Just response)
    post "/compile" $ do
      code <- param "code"
      response <- lift $ compile preludeModules code
      page Nothing (Just code) (Just response)

term :: Term (IO ())
term = server <$> port

port :: Term Int
port = value $ opt 80 $ (optInfo [ "p", "port" ])
     { optDoc = "The port to listen on" }

termInfo :: TermInfo
termInfo = defTI
  { termName = "trypurescript"
  , version  = showVersion Paths.version
  , termDoc  = "Try PureScript in the browser"
  }

main :: IO ()
main = run (term, termInfo)

