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
import Control.Monad.IO.Class
import Control.Monad
import Data.String (fromString)
import Hedgehog.Main (defaultMain)

genSymbol :: Gen Raw.Symbol
genSymbol = do
  s <- Gen.lower
  pure (fromString [s])

prop_symbols_deduplicate :: Property
prop_symbols_deduplicate = property do
  sg <- liftIO Raw.stackGraphNew
  n1 <- forAll Gen.lower
  handles <- liftIO $ Raw.stackGraphAddSymbols sg [fromString [n1], fromString [n1], fromString [n1, n1]]

  annotateShow handles

  let [a1, a2, b1] = handles

  a1 === a2
  b1 /== a1
  b1 /== a2

prop_symbols_correspond_one :: Property
prop_symbols_correspond_one = property do
  sg <- liftIO Raw.stackGraphNew
  xs1 <- liftIO $ Raw.stackGraphSymbolsRaw sg
  xs1 === []


  sym <- forAll genSymbol
  hdls <- liftIO . Raw.stackGraphAddSymbols sg $ [sym]
  xs <- liftIO $ Raw.stackGraphSymbolsRaw sg
  let [x1] = xs
  annotateShow xs
  annotateShow hdls
  x1 === sym

main :: IO ()
main = defaultMain [checkParallel $$(discover)]
