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

{-# LANGUAGE OverloadedStrings #-}

module Main (
    main
) where

import Web.Scotty
import qualified Language.PureScript as P

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

data Compiled = Compiled { js      :: String
                         , externs :: String
                         }

data Response = Response (Either String Compiled)

compile :: String -> IO Response
compile input | length input > 5000 = return $ Response $ Left "Please limit your input to 5000 characters"
compile input = do
  case P.runIndentParser P.parseModules input of
    Left parseError -> do
      return $ Response $ Left $ show parseError
    Right modules -> do
      case P.compile modules of
        Left error ->
          return $ Response $ Left error
        Right (js, externs, _) ->
          return $ Response $ Right $ Compiled js externs

str :: String -> String
str = id

mono :: H.Html -> H.Html
mono h = h ! A.class_ "mono"

examplesJs :: String
examplesJs = unlines
  [ "$('#examples').change(function() {"
  , "  var name = $('#examples').val();"
  , "  if (name) {"
  , "    window.location = '/example/' + name;"
  , "  }"
  , "});"
  ]

css :: String
css = unlines
  [ "body { font-family: 'Lato', sans-serif; color: #404040; margin: 0; }"
  , ".mono { font-family: 'Ubuntu Mono', monospace; white-space: pre; word-break: break-all; word-wrap: break-word; }"
  , ".header { margin: 0; background: #202028; box-shadow: 0 0 10px #808080; color: #E0E0E0; }"
  , ".splitter { margin: 0; height: 5px; background: #606068; }"
  , ".center { margin: 0 auto; padding: 20px; }"
  , "a { color: #808080; }"
  , "button { background: #d0d0d0; color: #606060; padding-top: 3px; padding-bottom: 3px; font-weight: bold;  border-radius: 1px; border: 1px solid #c0c0c0; box-shadow: 1px 1px 0 0 #ffffff inset; padding-left: 15px; padding-right: 15px; cursor: pointer; }"
  , "button:hover { background: #e0e0e0; }"
  , "#code, #js { margin: 10px; }"]

gaq :: String
gaq = unlines
  [ "var _gaq = _gaq || [];"
  , "_gaq.push(['_setAccount', 'UA-33896432-1']);"
  , "_gaq.push(['_trackPageview']);"
  , "(function() {"
  , " var ga = document.createElement('script'); ga.type = 'text/javascript'; ga.async = true;"
  , " ga.src = ('https:' == document.location.protocol ? 'https://ssl' : 'http://www') + '.google-analytics.com/ga.js';"
  , " var s = document.getElementsByTagName('script')[0]; s.parentNode.insertBefore(ga, s);"
  , "})();" ]

ace :: Bool -> String
ace js = unlines $
  [ "var editor = ace.edit('code');"
  , "editor.setTheme('ace/theme/dawn');"
  , "editor.renderer.setShowGutter(false);"
  , "var session = editor.getSession();"
  , "session.setMode('ace/mode/haskell');"
  , "session.setValue($('#textarea').val());"
  , "session.setUseWrapMode(true);"
  , "session.on('change', function(){"
  , "  $('#textarea').val(editor.getSession().getValue());"
  , "});"
  , "if ($('#js')[0]) {"
  , "  var js = ace.edit('js');"
  , "  js.setTheme('ace/theme/dawn');"
  , "  js.renderer.setShowGutter(false);"
  , "  js.setReadOnly(true);"
  , "  var session = js.getSession();"
  , "  session.setUseWrapMode(true);"
  ]
  ++ (if js then
        [ "  session.setMode('ace/mode/javascript');" ]
      else
        []) ++
  [ "}"
  , "function setHeight() {"
  , "  var top = $('#code').offset().top;"
  , "  var tot = $(window).height();"
  , "  var height = Math.max(tot - top - 50, 200);"
  , "  $('#code').height(height + 'px');"
  , "  $('#js').height(height + 'px');"
  , "}"
  , "$(setHeight);"
  , "$(window).on('resize', setHeight);" ]

examples :: [(String, (String, String))]
examples =
  [ ("adt",
      ("Algebraic Data Types",
        unlines [ "module ADTs where"
                , ""
                , "data Person = Person { name :: String, age :: Number }"
                , ""
                , "foreign import numberToString :: Number -> String"
                , ""
                , "showPerson (Person { name = name, age = age }) ="
                , "  name ++ \", aged \" ++ numberToString age"
                ]))
  , ("ops",
      ("Operators",
        unlines [ "module Operators where"
                , ""
                , "infixl 5 |>"
                , ""
                , "(|>) :: forall a b c. (a -> b) -> (b -> c) -> a -> c"
                , "(|>) f g a = g (f a)"
                , ""
                , "foreign import foo :: String -> Number"
                , "foreign import bar :: Number -> Boolean"
                , ""
                , "test = foo |> bar"
                ]))
  , ("arrays",
      ("Arrays",
        unlines [ "module Arrays where"
                , ""
                , "sum (x:xs) = x + sum xs"
                , "sum _ = 0"
                , ""
                , "sumOfProducts (x : y : xs) = x * y + sumOfProducts xs"
                , "sumOfProducts _ = 0"
                ]))
  , ("rows",
      ("Row Polymorphism",
        unlines [ "module RowPolymorphism where"
                , ""
                , "showPerson o = o.lastName ++ \", \" ++ o.firstName"
                ]))
  , ("ffi",
      ("FFI",
        unlines [ "module FFI where"
                , ""
                , "foreign import data IO :: * -> *"
                , ""
                , "foreign import console :: { log :: String -> IO { } }"
                , ""
                , "main = console.log \"Hello World!\""
                ]))
  , ("blocks",
      ("Mutable Variables",
        unlines [ "module Mutable where"
                , ""
                , "collatz :: Number -> Number"
                , "collatz n ="
                , "  { "
                , "    var m = n;"
                , "    var count = 0;"
                , "    while (m > 1) {"
                , "      if (m % 2 == 0) {"
                , "        m = m / 2;"
                , "      } else {"
                , "        m = 3 * m + 1;"
                , "      }"
                , "      count = count + 1;"
                , "    }"
                , "    return count;"
                , "  }"
                ]))
  , ("modules",
      ("Modules",
        unlines [ "module M1 where"
                , ""
                , "incr :: Number -> Number"
                , "incr x = x + 1"
                , ""
                , "module M2 where"
                , ""
                , "test = M1.incr 10"
                ]))
  , ("rank2",
      ("Rank N Types",
       unlines [ "module RankNTypes where"
               , ""
               , "type Nat = forall a. a -> (a -> a) -> a"
               , ""
               , "zero :: Nat"
               , "zero a _ = a"
               , ""
               , "succ :: Nat -> Nat"
               , "succ n a f = f (n a f)"
               , ""
               , "type Lens a b = forall f. (a -> f a) -> b -> f b"
               , ""
               , "compose :: forall a b c. Lens a b -> Lens b c -> Lens a c"
               , "compose l1 l2 f = l2 (l1 f)"
               ]))
  , ("recursion",
      ("Recursion",
       unlines [ "module Recursion where"
               , ""
               , "isOdd :: Number -> Boolean"
               , "isOdd 0 = false"
               , "isOdd n = isEven (n - 1)"
               , ""
               , "isEven :: Number -> Boolean"
               , "isEven 0 = true"
               , "isEven n = isOdd (n - 1)"
               ]))
  , ("do",
      ("Do Notation",
       unlines [ "module DoNotation where"
               , ""
               , "data Maybe a = Nothing | Just a"
               , ""
               , "bindMaybe Nothing _ = Nothing"
               , "bindMaybe (Just a) f = f a"
               , ""
               , "maybe = { ret: Just, bind: bindMaybe }"
               , ""
               , "isEven n | n % 2 == 0 = Just {}"
               , "isEven _ = Nothing"
               , ""
               , "evenSum a b = maybe do"
               , "  n <- a"
               , "  m <- b"
               , "  let sum = n + m"
               , "  isEven sum"
               , "  return sum"
               ]))
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
	       H.script ! A.type_ "text/javascript" $ preEscapedToHtml (ace success)

responseToJs :: Maybe Response -> (Bool, String)
responseToJs Nothing = (False, "")
responseToJs (Just (Response (Left err))) = (False, err)
responseToJs (Just (Response (Right (Compiled "" "")))) = (False, "Please enter some input")
responseToJs (Just (Response (Right (Compiled js _)))) = (True, js)

server :: Int -> IO ()
server port = scotty port $ do
  get "/" $ do
    page Nothing (Just "-- Type PureScript code here and click 'Compile' ...\r\n-- \r\n-- Or select an example from the list at the top right of the page") Nothing
  get "/example/:name" $ do
    name <- param "name"
    case lookup name examples of
      Nothing -> raise "No such example"
      Just (_, code) -> do
        response <- lift $ compile code
        page (Just name) (Just code) (Just response)
  post "/compile" $ do
    code <- param "code"
    response <- lift $ compile code
    page Nothing (Just code) (Just response)

term :: Term (IO ())
term = server <$> port

port :: Term Int
port = value $ opt 80 $ (optInfo [ "p", "port" ])
     { optDoc = "The port to listen on" }

termInfo :: TermInfo
termInfo = defTI
  { termName = "trypurescript"
  , version  = "0.1.0.0"
  , termDoc  = "Try PureScript in the browser"
  }

main :: IO ()
main = run (term, termInfo)

