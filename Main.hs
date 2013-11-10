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
import Language.PureScript

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
  case runIndentParser parseDeclarations input of
    Left parseError -> do
      return $ Response $ Left $ show parseError
    Right decls -> do
      case check (typeCheckAll decls) of
        Left typeError -> do
          return $ Response $ Left typeError
        Right (_, env) -> do
          let js = intercalate ";\n" . map (prettyPrintJS . optimize) . concat . mapMaybe (declToJs Nothing global) $ decls
          let externs = intercalate "\n" $ mapMaybe (externToPs 0 global env) decls
          return $ Response $ Right $ Compiled js externs

str :: String -> String
str = id

mono :: H.Html -> H.Html
mono h = h ! A.class_ "mono"

css :: String
css = "body { font-family: 'Lato', sans-serif } \
      \.mono { font-family: 'Ubuntu Mono', monospace; white-space: pre }"

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

examples :: [(String, (String, String))]
examples =
  [ ("adt",
      ("Algebraic Data Types",
        unlines [ "data Person = Person { name :: String, age :: Number }"
                , ""
                , "foreign import numberToString :: Number -> String"
                , ""
                , "showPerson = \\p -> case p of"
                , "  Person o -> o.name ++ \", aged \" ++ numberToString o.age"
                ]))
  , ("ops",
      ("Operators",
        unlines [ "infixl 5 |>"
                , ""
                , "(|>) :: forall a b c. (a -> b) -> (b -> c) -> a -> c"
                , "(|>) = \\f -> \\g -> \\a -> g (f a)"
                , ""
                , "foreign import foo :: String -> Number"
                , "foreign import bar :: Number -> Boolean"
                , ""
                , "test = foo |> bar"
                ]))
  , ("arrays",
      ("Arrays",
        unlines [ "sum = \\arr -> case arr of"
                , "  [x:xs] -> x + sum xs"
                , "  [] -> 0"
                , ""
                , "sumOfProducts = \\arr -> case arr of"
                , "  [x,y:xs] -> x * y + sum xs"
                , "  _ -> 0"
                ]))
  , ("rows",
      ("Row Polymorphism",
        unlines [ "showPerson = \\o -> o.lastName ++ \", \" ++ o.firstName"
                ]))
  , ("ffi",
      ("FFI",
        unlines [ "foreign import data IO :: * -> *"
                , ""
                , "foreign import console :: {"
                , "  log :: String -> IO { }"
                , "}"
                , ""
                , "main = console.log \"Hello World!\""
                ]))
  , ("blocks",
      ("Mutable Variables",
        unlines [ "collatz :: Number -> Number"
                , "collatz = \\n -> do"
                , "  var m = n"
                , "  var count = 0"
                , "  while m > 1:"
                , "    if m % 2 == 0:"
                , "      m = m / 2"
                , "    else:"
                , "      m = 3 * m + 1"
                , "    count = count + 1"
                , "  return count"
                ]))
  , ("modules",
      ("Modules",
        unlines [ "module Test where"
                , ""
                , "  incr :: Number -> Number"
                , "  incr = \\x -> x + 1"
                , ""
                , "test = Test:incr 10"
                ]))
  ]

page :: Maybe String -> Maybe Response -> ActionM ()
page input res = html $ renderHtml $ do
  H.docType
  H.html $ do
    H.head $ do
      H.title $ H.toHtml $ str "Try PureScript!"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "http://fonts.googleapis.com/css?family=Lato:300,400,700"
      H.link ! A.rel "stylesheet" ! A.type_ "text/css" ! A.href "http://fonts.googleapis.com/css?family=Ubuntu+Mono"
      H.style $ H.toHtml $ str css
      H.script ! A.type_ "text/javascript" $ preEscapedToHtml gaq
    H.body $ do
       H.h1 $ H.toHtml $ str "Try PureScript!"
       H.p $ H.toHtml $ str "Type PureScript code below and press 'Compile' to view the compiled Javascript."
       H.p $ mconcat [ H.a ! A.href "http://functorial.com/purescript" $ H.toHtml $ str "Documentation"
                     , H.toHtml $ str ", "
                     , H.a ! A.href "http://github.com/paf31/purescript" $ H.toHtml $ str "Compiler Source"
                     , H.toHtml $ str ", "
                     , H.a ! A.href "http://github.com/paf31/trypurescript" $ H.toHtml $ str "Try PureScript Source" ]
       H.h2 $ H.toHtml $ str "Examples"
       H.ul $ forM_ examples $ \(name, (title, _)) -> do
         H.li $ H.a ! A.href (fromString $ "/example/" ++ name) $ H.toHtml title
       H.h2 $ H.toHtml $ str "PureScript Code"
       H.form ! A.action "/compile" ! A.method "POST" $ do
           H.textarea ! A.name "code" ! A.rows "15" ! A.cols "100" $ maybe mempty (H.toHtml . str) input
           H.div $ H.button ! A.type_ "submit" $ H.toHtml $ str "Compile"
       case res of
         Nothing -> mempty
         Just (Response (Left err)) -> do
           H.h1 $ H.toHtml $ str "Error!"
           mono $ H.p $ H.toHtml $ err
         Just (Response (Right (Compiled "" ""))) -> do
           H.h1 $ H.toHtml $ str "Error!"
           mono $ H.p $ H.toHtml $ str "Please enter some input"
         Just (Response (Right (Compiled js exts))) -> do
           when (not . null $ js) $ do
             H.h1 $ H.toHtml $ str "Generated Javascript"
             mono $ H.p $ H.toHtml js
           when (not . null $ exts) $ do
             H.h1 $ H.toHtml $ str "Externs"
             mono $ H.p $ H.toHtml exts

server :: Int -> IO ()
server port = scotty port $ do
  get "/" $ do
    page Nothing Nothing
  get "/example/:name" $ do
    name <- param "name"
    case lookup name examples of
      Nothing -> raise "No such example"
      Just (_, code) -> do
        response <- lift $ compile code
        page (Just code) (Just response)
  post "/compile" $ do
    code <- param "code"
    response <- lift $ compile code
    page (Just code) (Just response)

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

