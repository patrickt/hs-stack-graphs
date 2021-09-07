{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hedgehog
import StackGraph.IR qualified as IR
import StackGraph.Raw qualified as Raw
import Control.Monad.IO.Class
import Control.Monad
import Hedgehog.Main (defaultMain)

prop_can_create_symbols :: Property
prop_can_create_symbols = withTests 1 $ property do
  sg <- liftIO Raw.stackGraphNew
  (handles, content) <- liftIO $ do
    handles <- Raw.stackGraphAddSymbols sg ["a", "a"]
    content <- Raw.stackGraphSymbols sg
    pure (handles, content)

  annotateShow handles

  let [a1, a2] = handles
  a1 === a2
  -- a1 /== b
  -- a1 /== c
  -- a2 /== b
  -- a2 /== c





main :: IO ()
main = defaultMain [checkParallel $$(discover)]
