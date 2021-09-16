{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module StackGraph.Node where

import Data.Generics.Sum.Constructors
import GHC.Generics (Generic)
import Optics
import StackGraph.Handle
import StackGraph.Symbol

data File = File

data Id = Id {parentFile :: Handle File, localId :: Handle Id}

data Node
  = Root
  | JumpTo
  | Node Kind Id
  deriving stock (Generic)

_Root, _JumpTo :: Prism' Node ()
_Root = _Ctor @"Root"
_JumpTo = _Ctor @"JumpTo"

data Kind
  = Scope ScopeOp
  | Pop Entity (Handle Symbol) (IsA 'Reference)
  | Push Entity (Handle Symbol) (IsA 'Definition)

data ScopeOp = Drop | Export | Internal

data IsA (t :: Clickable) = Yes | No

data Clickable = Reference | Definition

data Entity = Symbol | Scoped
