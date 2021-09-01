{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Foreign.C.Types
import Foreign.C.String
import Foreign
import StackGraph.Foreign
import GHC.Records
import StackGraph.IR qualified as IR

someFunc :: IO ()
someFunc = do
  IR.withStackGraph $ \sg -> do
    handles <- IR.stackGraphAddSymbols sg ["foo", "bar"]
    print handles
    syms <- IR.stackGraphSymbols sg
    print syms
