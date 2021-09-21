{-# LANGUAGE ImportQualifiedPost #-}

module Main where

import Control.Monad
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Gauge
import StackGraph.C qualified as SG
import System.Random

main :: IO ()
main = do
  setStdGen (mkStdGen 8675309)
  syms <- replicateM 5000 $ do
    chrs <- replicateM 5 (randomRIO ('A', 'z'))
    pure (B.pack chrs)

  defaultMain
    [ bgroup
        "symbols"
        [ bgroup
            "insert-fetch"
            [ bench "30" $ whnfAppIO symInsertFetch (take 30 syms),
              bench "300" $ whnfAppIO symInsertFetch (take 300 syms),
              bench "3000" $ whnfAppIO symInsertFetch (take 3000 syms)
            ]
        ]
    ]

symInsertFetch :: [ByteString] -> IO [ByteString]
symInsertFetch bs = do
  sg <- SG.new
  SG.addSymbols sg bs
  SG.symbols sg
