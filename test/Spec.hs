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
import StackGraph.Manual as Man
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
  sg <- liftIO Man.stackGraphNew
  n1 <- forAll (Gen.utf8 (Range.linear 1 3) Gen.lower)
  handles <- liftIO $ Man.stackGraphAddSymbols sg [n1, n1, n1 <> n1]
  annotateShow handles

  let [a1, a2, b1] = handles

  a1 === a2
  b1 /== a1
  b1 /== a2

prop_files_comparable :: Property
prop_files_comparable = property do
  sg <- liftIO Man.stackGraphNew
  n1 <- forAll (Gen.utf8 (Range.linear 1 3) Gen.ascii)
  handles <- liftIO $ Man.stackGraphAddFiles sg [n1, n1, n1 <> n1]
  annotateShow handles

  let [a1, a2, b1] = handles

  a1 === a2
  b1 /== a1
  b1 /== a2

prop_syms_deduplicate :: Property
prop_syms_deduplicate = withTests 100 $ property do
  sg <- liftIO Man.stackGraphNew
  sym <- forAll $ Gen.list (Range.linear 1 10000) (Gen.utf8 (Range.linear 1 10) Gen.ascii)

  liftIO $ Man.stackGraphAddSymbols sg sym
  xs <- liftIO $ Man.stackGraphSymbols sg

  length (nub sym) === length xs

prop_files_deduplicate :: Property
prop_files_deduplicate = withTests 100 $ property do
  sg <- liftIO Man.stackGraphNew
  sym <- forAll $ Gen.list (Range.linear 1 100) (Gen.utf8 (Range.linear 1 10) Gen.ascii)

  hdls <- liftIO $ Man.stackGraphAddFiles sg sym
  xs <- liftIO $ Man.stackGraphFiles sg

  length (nub sym) === length xs

prop_syms_roundtrip :: Property
prop_syms_roundtrip = withTests 100 $ property do
  sg <- liftIO Man.stackGraphNew
  sym <- forAll $ Gen.utf8 (Range.linear 1 10) Gen.ascii
  let test = [sym, sym <> sym]

  hdls <- liftIO $ Man.stackGraphAddSymbols sg test
  done <- liftIO $ Man.stackGraphSymbols sg

  test === Vector.toList done

prop_files_roundtrip :: Property
prop_files_roundtrip = withTests 100 $ property do
  sg <- liftIO Man.stackGraphNew
  sym <- forAll $ Gen.utf8 (Range.linear 1 10) Gen.ascii
  let test = [sym, sym <> sym]

  hdls <- liftIO $ Man.stackGraphAddFiles sg test
  done <- liftIO $ Man.stackGraphFiles sg

  test === Vector.toList done

main :: IO ()
main = do
  start <- getCurrentTime
  defaultMain [checkSequential $$(discover)]
  end <- getCurrentTime
  let diff = diffUTCTime end start
  putStrLn ("Took " <> show (nominalDiffTimeToSeconds diff) <> " sec")
