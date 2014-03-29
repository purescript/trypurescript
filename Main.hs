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

preludeFilename :: IO FilePath
preludeFilename = Paths.getDataFileName "prelude/prelude.purs"

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
        unlines [ "module Main where"
                , ""
                , "import Prelude"
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
        unlines [ "module Main where"
                , ""
                , "import Prelude ()"
                , ""
                , "infixl 5 >>>"
                , ""
                , "(>>>) :: forall a b c. (a -> b) -> (b -> c) -> a -> c"
                , "(>>>) f g a = g (f a)"
                , ""
                , "foreign import foo :: String -> Number"
                , "foreign import bar :: Number -> Boolean"
                , ""
                , "test = foo >>> bar"
                ]))
  , ("arrays",
      ("Arrays",
        unlines [ "module Main where"
                , ""
                , "import Prelude"
                , ""
                , "sum (x:xs) = x + sum xs"
                , "sum _ = 0"
                , ""
                , "sumOfProducts (x : y : xs) = x * y + sumOfProducts xs"
                , "sumOfProducts _ = 0"
                ]))
  , ("rows",
      ("Row Polymorphism",
        unlines [ "module Main where"
                , ""
                , "import Prelude"
                , ""
                , "showPerson o = o.lastName ++ \", \" ++ o.firstName"
                ]))
  , ("ffi",
      ("FFI",
        unlines [ "module Main where"
                , ""
                , "foreign import data IO :: * -> *"
                , ""
                , "foreign import log \"function log(s) { return function() { console.log(s) }; }\" :: String -> IO { }"
                , ""
                , "main = log \"Hello World!\""
                ]))
  , ("blocks",
      ("Mutable Variables",
        unlines [ "module Main where"
                , ""
                , "import Prelude"
                , "import Control.Monad.Eff"
                , "import Control.Monad.ST"
                , ""
                , "collatz :: Number -> Number"
                , "collatz n = runPure (runST (do"
                , "  r <- newSTRef n"
                , "  count <- newSTRef 0"
                , "  untilE $ do"
                , "    modifySTRef count $ (+) 1"
                , "    m <- readSTRef r"
                , "    writeSTRef r $ if m % 2 == 0 then m / 2 else 3 * m + 1"
                , "    return $ m == 1"
                , "  readSTRef count))"
                ]))
  , ("modules",
      ("Modules",
        unlines [ "module M1 where"
                , ""
                , "import Prelude"
                , ""
                , "incr :: Number -> Number"
                , "incr x = x + 1"
                , ""
                , "module Main where"
                , ""
                , "test = M1.incr 10"
                ]))
  , ("rank2",
      ("Rank N Types",
       unlines [ "module Main where"
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
       unlines [ "module Main where"
               , ""
               , "import Prelude"
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
       unlines [ "module Main where"
               , ""
               , "import Prelude"
               , ""
               , "data Maybe a = Nothing | Just a"
               , ""
               , "instance monadMaybe :: Prelude.Monad Maybe where"
               , "  return = Just"
               , "  (>>=) Nothing _ = Nothing"
               , "  (>>=)  (Just a) f = f a"
               , ""
               , "isEven n | n % 2 == 0 = Just {}"
               , "isEven _ = Nothing"
               , ""
               , "evenSum a b = do"
               , "  n <- a"
               , "  m <- b"
               , "  let sum = n + m"
               , "  isEven sum"
               , "  return sum"
               ]))
  , ("tco",
      ("Tail-Call Elimination",
       unlines [ "module Main where"
               , ""
               , "import Prelude"
               , ""
               , "factHelper prod 0 = prod"
               , "factHelper prod n = factHelper (prod * n) (n - 1)"
               , ""
               , "fact = factHelper 1"
               ]))
  , ("typeclasses",
      ("Type Classes",
       unlines [ "module Main where"
               , ""
               , "import Prelude ((++))"
               , ""
               , "class Show a where"
               , "  show :: a -> String"
               , ""
               , "instance showString :: Show String where"
               , "  show s = s"
               , ""
               , "instance showBoolean :: Show Boolean where"
               , "  show true = \"true\""
               , "  show false = \"false\""
               , ""
               , "instance showArray :: (Show a) => Show [a] where"
               , "  show arr = \"[\" ++ go arr ++ \"]\""
               , "    where"
               , "    go :: forall a. (Show a) => [a] -> String"
               , "    go [] = \"\""
               , "    go [x] = show x"
               , "    go (x:xs) = show x ++ \", \" ++ go xs"
               , ""
               , "test = show [true, false]"
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
server port = do
  prelude <- preludeFilename >>= U.readFile
  let preludeModules = either (error . show) id $ P.runIndentParser "" P.parseModules prelude
  scotty port $ do
    get "/" $ do
      page Nothing (Just (unlines [ "-- Type PureScript code here and click 'Compile' ..."
                                  , "-- "
                                  , "-- Or select an example from the list at the top right of the page"
                                  , ""
                                  , "module Main where"
                                  , ""
                                  , "import Debug.Trace"
                                  , ""
                                  , "main = trace \"Hello, World!\""
                                  ])) Nothing
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

