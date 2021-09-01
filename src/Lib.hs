{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
  sg <- stackGraphNew
  withCStringLen "testlent" $ \(str, len) ->
    withArray [4, 4] $ \lengths -> do
      handles <- mallocArray @SymbolHandle 1
      stackGraphAddSymbols sg 1 str lengths handles
      x :: [SymbolHandle] <- peekArray 1 handles
      syms <- stackGraphSymbols sg
      print syms
