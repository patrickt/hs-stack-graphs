{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hedgehog
import Data.Time.Clock
import Gen qualified
import Hedgehog.Gen qualified as Gen
import StackGraph.C qualified as SG
import Data.ByteString (ByteString)
import Data.List (nub)
import Hedgehog.Range qualified as Range
import Control.Monad.IO.Class
import Control.Monad
import Data.String (fromString)
import Data.Vector.Generic qualified as Vector
import Hedgehog.Main (defaultMain)

prop_symbols_comparable :: Property
prop_symbols_comparable = property do
  sg <- liftIO SG.new
  n1 <- forAll (Gen.utf8 (Range.linear 1 3) Gen.lower)
  handles <- liftIO $ SG.addSymbols sg [n1, n1, n1 <> n1]
  annotateShow handles

  let [a1, a2, b1] = handles

  a1 === a2
  b1 /== a1
  b1 /== a2

prop_files_comparable :: Property
prop_files_comparable = property do
  sg <- liftIO SG.new
  n1 <- forAll (Gen.utf8 (Range.linear 1 3) Gen.ascii)
  handles <- liftIO $ SG.addFiles sg [n1, n1, n1 <> n1]
  annotateShow handles

  let [a1, a2, b1] = handles

  a1 === a2
  b1 /== a1
  b1 /== a2

prop_syms_deduplicate :: Property
prop_syms_deduplicate = withTests 100 $ property do
  sg <- liftIO SG.new
  sym <- forAll $ Gen.list (Range.linear 1 10000) (Gen.utf8 (Range.linear 1 10) Gen.ascii)

  liftIO $ SG.addSymbols sg sym
  xs <- liftIO $ SG.symbols sg

  length (nub sym) === length xs

prop_files_deduplicate :: Property
prop_files_deduplicate = withTests 100 $ property do
  sg <- liftIO SG.new
  sym <- forAll $ Gen.list (Range.linear 1 100) (Gen.utf8 (Range.linear 1 10) Gen.ascii)

  hdls <- liftIO $ SG.addFiles sg sym
  xs <- liftIO $ SG.files sg

  length (nub sym) === length xs

prop_syms_roundtrip :: Property
prop_syms_roundtrip = withTests 100 $ property do
  sg <- liftIO SG.new
  sym <- forAll $ Gen.utf8 (Range.linear 1 10) Gen.ascii
  let test = [sym, sym <> sym]

  hdls <- liftIO $ SG.addSymbols sg test
  done <- liftIO $ SG.symbols sg

  test === done

prop_files_roundtrip :: Property
prop_files_roundtrip = withTests 100 $ property do
  sg <- liftIO SG.new
  sym <- forAll $ Gen.utf8 (Range.linear 1 10) Gen.ascii
  let test = [sym, sym <> sym]

  hdls <- liftIO $ SG.addFiles sg test
  done <- liftIO $ SG.files sg

  test === Vector.toList done

main :: IO ()
main = do
  start <- getCurrentTime
  defaultMain [checkSequential $$(discover)]
  end <- getCurrentTime
  let diff = diffUTCTime end start
  putStrLn ("Took " <> show (nominalDiffTimeToSeconds diff) <> " sec")
