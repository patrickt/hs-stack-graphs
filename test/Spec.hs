{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hedgehog
import Data.Time.Clock
import Hedgehog.Gen qualified as Gen
import StackGraph.Manual as Man
import Data.ByteString (ByteString)
import Data.List (nub)
import Hedgehog.Range qualified as Range
import Control.Monad.IO.Class
import Control.Monad
import Data.String (fromString)
import Hedgehog.Main (defaultMain)

prop_symbols_deduplicate :: Property
prop_symbols_deduplicate = property do
  sg <- liftIO Man.stackGraphNew
  n1 <- forAll (Gen.utf8 (Range.linear 1 3) Gen.ascii)
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
prop_syms_roundtrip = withTests 10000 $ property do
  sg <- liftIO Man.stackGraphNew
  sym <- forAll $ Gen.list (Range.linear 1 100) (Gen.utf8 (Range.linear 1 10) Gen.ascii)

  hdls <- liftIO $ Man.stackGraphAddSymbols sg sym
  xs <- liftIO $ Man.stackGraphSymbols sg

  length (nub sym) === length xs

main :: IO ()
main = do
  start <- getCurrentTime
  defaultMain [checkSequential $$(discover)]
  end <- getCurrentTime
  let diff = diffUTCTime end start
  putStrLn ("Took " <> show (nominalDiffTimeToSeconds diff) <> " sec")
