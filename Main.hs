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

-- | These need to be topologically sorted by dependencies, hence we don't just use
-- 'getDirectoryContents'.
externsFiles :: [FilePath]
externsFiles =
  [ "Math"
  , "Prelude"
  , "Data.Int.Bits"
  , "Control.Lazy"
  , "Control.Apply"
  , "Control.Extend"
  , "Control.Bind"
  , "Control.Alt"
  , "Data.Function"
  , "Data.Functor.Invariant"
  , "Data.Monoid"
  , "Control.Monad.Eff"
  , "Data.Functor"
  , "Data.Bifunctor"
  , "Control.Monad"
  , "Control.Plus"
  , "Data.Monoid.Endo"
  , "Control.Biapply"
  , "Control.Alternative"
  , "Control.Comonad"
  , "Data.Monoid.Conj"
  , "Data.Monoid.Additive"
  , "Data.Monoid.Dual"
  , "Data.Monoid.Disj"
  , "Control.Monad.Eff.Console"
  , "Control.Monad.ST"
  , "Data.Lazy"
  , "Control.Monad.Eff.Class"
  , "Control.Monad.Eff.Unsafe"
  , "Data.Monoid.Multiplicative"
  , "Control.MonadPlus"
  , "Control.Monad.Eff.Console.Unsafe"
  , "Control.Biapplicative"
  , "Data.Bifunctor.Joker"
  , "Data.Bifunctor.Flip"
  , "Data.Bifunctor.Wrap"
  , "Data.Maybe"
  , "Data.Bifunctor.Clown"
  , "Data.Bifunctor.Product"
  , "Data.Bifunctor.Join"
  , "Data.Bifoldable"
  , "Data.Bitraversable"
  , "Data.Maybe.Last"
  , "Data.Maybe.Unsafe"
  , "Control.Monad.Eff.Exception"
  , "Data.Maybe.First"
  , "Data.Array.ST"
  , "Data.Int"
  , "Control.Monad.Eff.Exception.Unsafe"
  , "Graphics.Canvas"
  , "Graphics.Drawing.Color"
  , "Data.Foldable"
  , "Graphics.Drawing.Font"
  , "Data.Traversable"
  , "Data.Tuple"
  , "Data.Tuple.Nested"
  , "Data.Unfoldable"
  , "Data.Array"
  , "Data.List"
  , "Data.List.Lazy"
  , "Data.Array.Unsafe"
  , "Data.List.ZipList"
  , "Data.List.Unsafe"
  , "Graphics.Drawing"
  ]

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
