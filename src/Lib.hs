{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Lib
    ( someFunc
    ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign
import StackGraph.Foreign
import GHC.Records

someFunc :: IO ()
someFunc = do
  x <- stackGraphNew
  syms <- stackGraphSymbols x
  print x
  print (getField @"count" syms)
