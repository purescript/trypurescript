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
      code <- T.unpack . T.decodeUtf8 . BL.toStrict <$> body
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
  [ "Data.NaturalTransformation"
  , "Global"
  , "Data.Functor.Contravariant"
  , "Data.Int.Bits"
  , "Math"
  , "Global.Unsafe"
  , "Data.String.Unsafe"
  , "Unsafe.Coerce"
  , "Prelude"
  , "Type.Proxy"
  , "Data.Functor"
  , "Data.Bifunctor"
  , "Control.Monad"
  , "Control.Apply"
  , "Data.Monoid"
  , "Control.Monad.Trans"
  , "Data.Char"
  , "Data.Function"
  , "Control.Monad.Reader.Class"
  , "Control.Monad.Eff"
  , "Data.Validation"
  , "Control.Extend"
  , "Control.Alt"
  , "Data.Profunctor"
  , "Data.Exists"
  , "Data.Functor.Invariant"
  , "Control.Lazy"
  , "Control.Monad.Cont.Class"
  , "Data.Predicate"
  , "Control.Bind"
  , "Control.Biapply"
  , "Data.Op"
  , "Control.Plus"
  , "Control.Monad.Eff.Ref"
  , "Control.Monad.Eff.Class"
  , "Control.Monad.Eff.Unsafe"
  , "Control.Monad.ST"
  , "Control.Monad.Eff.Console"
  , "Data.Profunctor.Closed"
  , "Data.Monoid.Endo"
  , "Data.Comparison"
  , "Control.Comonad"
  , "Control.Alternative"
  , "Control.Monad.Eff.Console.Unsafe"
  , "Control.Monad.Eff.Ref.Unsafe"
  , "Data.Monoid.Disj"
  , "Data.Lazy"
  , "Data.Monoid.Multiplicative"
  , "Control.Comonad.Trans"
  , "Data.Monoid.Conj"
  , "Data.Yoneda"
  , "Data.Coyoneda"
  , "Data.Monoid.Additive"
  , "Data.Monoid.Dual"
  , "Data.Equivalence"
  , "Control.Biapplicative"
  , "Control.MonadPlus"
  , "Control.Comonad.Traced.Trans"
  , "Data.Validation.Semiring"
  , "Data.Bifunctor.Product"
  , "Data.Bifunctor.Join"
  , "Data.Bifunctor.Clown"
  , "Data.Bifunctor.Joker"
  , "Data.Bifunctor.Wrap"
  , "Data.Bifoldable"
  , "Data.Bifunctor.Flip"
  , "Data.Maybe"
  , "Data.Bitraversable"
  , "Data.Maybe.First"
  , "Data.Maybe.Last"
  , "Control.Monad.Eff.Exception"
  , "Data.Array.ST"
  , "Data.Maybe.Unsafe"
  , "Data.StrMap.ST"
  , "Data.String"
  , "Data.Int"
  , "Control.Monad.Eff.Exception.Unsafe"
  , "Control.Monad.Eff.Random"
  , "Data.Foldable"
  , "Data.String.Regex"
  , "Test.QuickCheck.LCG"
  , "Data.Traversable"
  , "Data.Const"
  , "Data.Identity"
  , "Data.Either"
  , "Data.Tuple"
  , "Control.Comonad.Traced"
  , "Data.Distributive"
  , "Data.Either.Nested"
  , "Data.Profunctor.Cochoice"
  , "Control.Monad.Error.Class"
  , "Data.Functor.Coproduct"
  , "Data.Foreign"
  , "Data.Profunctor.Choice"
  , "Data.Either.Unsafe"
  , "Control.Monad.Rec.Class"
  , "Data.Inject"
  , "Data.Functor.Contravariant.Divisible"
  , "Control.Comonad.Store.Trans"
  , "Control.Comonad.Env.Trans"
  , "Data.Profunctor.Strong"
  , "Data.Profunctor.Costrong"
  , "Control.Monad.State.Class"
  , "Control.Comonad.Traced.Class"
  , "Data.Array"
  , "Control.Monad.Writer.Class"
  , "Data.Unfoldable"
  , "Data.Tuple.Nested"
  , "Data.Foreign.NullOrUndefined"
  , "Data.Foreign.Keys"
  , "Data.Foreign.Index"
  , "Data.Foreign.Undefined"
  , "Data.Foreign.Null"
  , "Data.Profunctor.Star"
  , "Control.Comonad.Store"
  , "Control.Comonad.Store.Class"
  , "Control.Comonad.Env"
  , "Control.Comonad.Env.Class"
  , "Control.Monad.Cont.Trans"
  , "Control.Monad.Writer.Trans"
  , "Control.Monad.Reader.Trans"
  , "Control.Monad.RWS.Class"
  , "Control.Monad.List.Trans"
  , "Data.Enum"
  , "Control.Monad.State.Trans"
  , "Data.List.Lazy"
  , "Data.List"
  , "Control.Monad.Except.Trans"
  , "Control.Monad.RWS.Trans"
  , "Control.Monad.Maybe.Trans"
  , "Control.Parallel"
  , "Data.Array.Unsafe"
  , "Data.Foreign.Class"
  , "Data.Generic"
  , "Control.Monad.Reader"
  , "Control.Monad.Writer"
  , "Control.Monad.Except"
  , "Control.Monad.RWS"
  , "Control.Monad.State"
  , "Data.List.ZipList"
  , "Data.CatQueue"
  , "Data.Map"
  , "Data.List.Unsafe"
  , "Data.Semiring.Free"
  , "Data.StrMap"
  , "Test.QuickCheck.Gen"
  , "Data.CatList"
  , "Control.Monad.Free"
  , "Data.StrMap.Unsafe"
  , "Data.StrMap.ST.Unsafe"
  , "Test.QuickCheck.Arbitrary"
  , "Control.Monad.Trampoline"
  , "Control.Comonad.Cofree"
  , "Test.QuickCheck.Data.AlphaNumString"
  , "Test.QuickCheck"
  , "Test.QuickCheck.Data.ApproxNumber"
  , "Data.Set"
  , "Data.Graph"
  ]
