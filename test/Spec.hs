{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hedgehog
import Hedgehog.Gen qualified as Gen
import StackGraph.IR qualified as IR
import StackGraph.Raw qualified as Raw
import StackGraph.Manual as Man
import Data.ByteString (ByteString)
import Control.Monad.IO.Class
import Control.Monad
import Data.String (fromString)
import Hedgehog.Main (defaultMain)

genSymbol :: Gen ByteString
genSymbol = do
  s <- Gen.lower
  pure (fromString [s, s, s])

prop_symbols_deduplicate :: Property
prop_symbols_deduplicate = property do
  sg <- liftIO Man.stackGraphNew
  n1 <- forAll genSymbol
  handles <- liftIO $ Man.stackGraphAddSymbols sg [n1, n1, n1 <> n1]
  annotateShow handles

  let [a1, a2, b1] = handles

  a1 === a2
  b1 /== a1
  b1 /== a2

prop_symbols_correspond_one :: Property
prop_symbols_correspond_one = withTests 1 $ property do
  sg <- liftIO Man.stackGraphNew
  xs1 <- liftIO $ Man.stackGraphSymbols sg
  xs1 === []


prop_syms_roundtrip :: Property
prop_syms_roundtrip = property do
  sg <- liftIO Man.stackGraphNew
  sym <- forAll genSymbol

  hdls <- liftIO . Man.stackGraphAddSymbols sg $ [sym]
  xs <- liftIO $ Man.stackGraphSymbols sg

  let [x1] = xs
  annotateShow xs
  annotateShow hdls
  x1 === sym

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
