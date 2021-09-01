{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Hedgehog
import StackGraph.IR qualified as IR
import Control.Monad.IO.Class
import Control.Monad

prop_can_create_symbols :: Property
prop_can_create_symbols = withTests 1 $ property do
  (handles, content) <- liftIO $ IR.withStackGraph \sg -> do
    handles <- IR.stackGraphAddSymbols sg ["a", "a", "b", "c", ""]
    content <- IR.stackGraphSymbols sg
    pure (handles, content)
  annotateShow handles
  let [a1, a2, b, c, empty1] = handles
  a1 === a2
  a1 /== b
  a1 /== c
  a2 /== b
  a2 /== c
  empty1 /== a1

  annotateShow content
  let [str1, str2, strb, strc, strempty] = content
  str1 === str2
  str1 /== strb
  str1 /== strc
  str2 /== strb
  str2 /== strc
  strempty /== str1




main :: IO ()
main = void (checkParallel $$(discover))
