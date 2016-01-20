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
import qualified Data.ByteString.Lazy.Char8 as BLC8
import           Data.List (foldl')
import           Data.String (fromString)

import           Control.Applicative ((<$>))
import           Control.Monad (unless)
import           Control.Monad.Logger (runLogger')
import           Control.Monad.Trans (lift)
import           Control.Monad.Error.Class (throwError)
import           Control.Monad.Trans.Except (runExceptT)
import           Control.Monad.Trans.Reader (runReaderT)

import qualified Language.PureScript as P
import qualified Language.PureScript.Externs as P
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

server :: FilePath -> Int -> IO ()
server externsPath port = do
  externs <- mapM (readExterns . (externsPath </>) . (++ ".json")) externsFiles

  let initEnv = foldl' (flip P.applyExternsFileToEnvironment) P.initEnvironment externs

  let compile :: String -> IO (Either String JS)
      compile input
        | length input > 20000 = return $ Left "Please limit your input to 20000 characters"
        | otherwise =
          case map snd <$> P.parseModulesFromFiles (const "<file>") [(undefined, input)] of
            Left parseError ->
              return $ Left $ P.prettyPrintMultipleErrors False parseError
            Right [m] | P.getModuleName m == P.ModuleName [P.ProperName "Main"] -> do
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
                Left errs -> return . Left . P.prettyPrintMultipleErrors False $ errs
                Right js -> return (Right js)
            Right [_] -> return $ Left "The name of the main module should be Main."
            Right _ -> return $ Left "Please define exactly one module called Main."

  scotty port $ do
    get "/" $
      Scotty.text "POST api.purescript.org/compile"
    post "/compile" $ do
      code <- BLC8.unpack <$> body
      response <- lift $ compile code
      case response of
        Left err ->
          Scotty.json $ A.object [ "error" .= err ]
        Right comp ->
          Scotty.json $ A.object [ "js" .= comp ]

main :: IO ()
main = do
  [externsPath, port] <- getArgs
  server externsPath (read port)

-- | These need to be topologically sorted by dependencies, hence we don't just use
-- 'getDirectoryContents'.
externsFiles :: [FilePath]
externsFiles =
  [ "DOM.XHR.Types"
  , "Control.Timer"
  , "DOM"
  , "Data.ArrayBuffer.Types"
  , "DOM.File.Types"
  , "Math"
  , "Data.String.Unsafe"
  , "Prelude"
  , "Unsafe.Coerce"
  , "Data.Int.Bits"
  , "Control.Bind"
  , "Control.Apply"
  , "Control.Monad.Reader.Class"
  , "Data.Bifunctor"
  , "Control.Monad"
  , "Network.HTTP.MimeType"
  , "Control.Alt"
  , "Control.Extend"
  , "Network.HTTP.ResponseHeader"
  , "Network.HTTP.StatusCode"
  , "Network.HTTP.Method"
  , "Data.Functor"
  , "Data.Monoid"
  , "Data.Functor.Invariant"
  , "Data.Char"
  , "Control.Monad.Trans"
  , "Control.Lazy"
  , "Control.Monad.Eff"
  , "Control.Monad.Cont.Class"
  , "Data.Function"
  , "Control.Biapply"
  , "Control.Plus"
  , "Network.HTTP.MimeType.Common"
  , "Network.HTTP.RequestHeader"
  , "Control.Comonad"
  , "Data.Monoid.Endo"
  , "Data.Monoid.Multiplicative"
  , "Data.Monoid.Additive"
  , "Data.Monoid.Dual"
  , "Data.Monoid.Disj"
  , "Control.Comonad.Trans"
  , "Control.Monad.ST"
  , "Data.Lazy"
  , "Control.Monad.Eff.Console"
  , "Data.Monoid.Conj"
  , "Control.Alternative"
  , "Control.Monad.Eff.Ref"
  , "Control.Monad.Eff.Unsafe"
  , "Control.Monad.Eff.Class"
  , "Control.Biapplicative"
  , "Control.Comonad.Traced.Trans"
  , "Control.Monad.Eff.Console.Unsafe"
  , "Control.MonadPlus"
  , "Control.Monad.Eff.Ref.Unsafe"
  , "Data.Bifunctor.Flip"
  , "Data.Bifunctor.Product"
  , "Data.Bifunctor.Join"
  , "Data.Bifunctor.Wrap"
  , "Data.Bifunctor.Clown"
  , "Data.Bifunctor.Joker"
  , "Data.Maybe"
  , "Data.Bifoldable"
  , "Data.Bitraversable"
  , "Data.Maybe.First"
  , "Text.Smolder.Markup"
  , "Data.Maybe.Last"
  , "Data.StrMap.ST"
  , "Data.Array.ST"
  , "Data.String"
  , "Data.Nullable"
  , "Control.Monad.Eff.Exception"
  , "Data.Maybe.Unsafe"
  , "Data.Int"
  , "Control.Monad.Eff.Exception.Unsafe"
  , "Graphics.Canvas"
  , "Graphics.Drawing.Color"
  , "Data.Foldable"
  , "Data.String.Regex"
  , "Text.Smolder.HTML.Attributes"
  , "Text.Smolder.HTML"
  , "Data.Traversable"
  , "Graphics.Drawing.Font"
  , "Signal"
  , "Signal.Channel"
  , "Signal.Time"
  , "Signal.DOM"
  , "Data.Either"
  , "Data.Identity"
  , "Data.Tuple"
  , "Control.Comonad.Traced"
  , "Data.Distributive"
  , "Control.Monad.Error.Class"
  , "Data.Either.Unsafe"
  , "Data.Foreign"
  , "Data.Either.Nested"
  , "Control.Monad.Rec.Class"
  , "Data.Tuple.Nested"
  , "Control.Comonad.Env.Trans"
  , "Data.Array"
  , "Control.Monad.Writer.Class"
  , "Control.Comonad.Store.Trans"
  , "Data.Unfoldable"
  , "Control.Monad.State.Class"
  , "Control.Comonad.Traced.Class"
  , "Data.Foreign.NullOrUndefined"
  , "Data.Foreign.Null"
  , "Data.Foreign.Index"
  , "Control.Monad.Aff"
  , "Data.Foreign.Undefined"
  , "Data.Foreign.Keys"
  , "Control.Monad.Cont.Trans"
  , "Control.Comonad.Env"
  , "Control.Comonad.Store.Class"
  , "Control.Comonad.Env.Class"
  , "Control.Comonad.Store"
  , "Control.Monad.RWS.Class"
  , "Control.Monad.Writer.Trans"
  , "Control.Monad.Reader.Trans"
  , "Control.Monad.State.Trans"
  , "Data.List"
  , "Data.Enum"
  , "Data.List.Lazy"
  , "Control.Monad.List.Trans"
  , "Control.Monad.Maybe.Trans"
  , "Control.Monad.Except.Trans"
  , "Control.Monad.RWS.Trans"
  , "Data.Array.Unsafe"
  , "Data.Foreign.Class"
  , "Control.Monad.Aff.AVar"
  , "Control.Monad.Aff.Console"
  , "Control.Monad.Aff.Unsafe"
  , "Control.Monad.Reader"
  , "Control.Monad.Writer"
  , "Control.Monad.State"
  , "DOM.Node.NodeType"
  , "DOM.Event.EventPhase"
  , "Control.Monad.Aff.Par"
  , "DOM.Event.Types"
  , "Control.Monad.Except"
  , "Control.Monad.RWS"
  , "Control.Monad.Aff.Class"
  , "Data.List.ZipList"
  , "Data.StrMap"
  , "Data.List.Unsafe"
  , "Data.Map"
  , "Graphics.Drawing"
  , "DOM.Event.EventTarget"
  , "DOM.Node.Types"
  , "DOM.Event.EventTypes"
  , "DOM.Node.NodeList"
  , "DOM.HTML.Types"
  , "DOM.Node.DocumentType"
  , "DOM.Node.Node"
  , "DOM.Node.Document"
  , "DOM.Node.ChildNode"
  , "DOM.Node.Element"
  , "DOM.Node.NonDocumentTypeChildNode"
  , "Flare"
  , "DOM.Node.ParentNode"
  , "DOM.Event.Event"
  , "DOM.Node.HTMLCollection"
  , "DOM.Node.NonElementParentNode"
  , "DOM.HTML.Location"
  , "DOM.HTML.Document"
  , "DOM.HTML.Window"
  , "DOM.HTML"
  , "DOM.HTML.Navigator"
  , "Data.StrMap.ST.Unsafe"
  , "Data.Argonaut.Core"
  , "Data.StrMap.Unsafe"
  , "Data.Argonaut.Parser"
  , "Data.Argonaut.Printer"
  , "Network.HTTP.Affjax.Response"
  , "Network.HTTP.Affjax.Request"
  , "Flare.Drawing"
  , "Network.HTTP.Affjax"
  , "Text.Smolder.Renderer.Util"
  , "Text.Smolder.Renderer.String"
  , "Flare.Smolder"
  ]
