{-# LANGUAGE DerivingStrategies #-}

module StackGraph.Symbol ( Symbol (..) ) where

import Data.ByteString (ByteString)

newtype Symbol = Symbol ByteString
  deriving stock (Eq, Show)
