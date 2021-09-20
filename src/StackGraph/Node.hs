{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module StackGraph.Node where

import Data.Generics.Product.Types qualified as Generic
import Data.Generics.Sum.Constructors
import GHC.Generics (Generic)
import Optics
import StackGraph.Handle
import StackGraph.Symbol
import StackGraph.Manual qualified as C
import Data.Kind (Type)
import Data.Coerce
import Control.Exception
import Foreign.C (CInt)
import Data.Generics.Sum.Typed
import Data.Maybe

data File = File

newtype Id = Id C.NodeId deriving stock Generic

data Node
  = Root
  | JumpTo
  | Node Kind Id
  deriving stock (Generic)

_Root, _JumpTo :: Prism' Node ()
_Root = _Ctor @"Root"
_JumpTo = _Ctor @"JumpTo"

data Kind
  = DropScopes
  | ExportedScope
  | InternalScope
  | PopScoped (Handle Symbol) (IsA 'Definition)
  | PopSymbol (Handle Symbol) (IsA 'Definition)
  | PushScoped (Handle Symbol) Id (IsA 'Reference)
  | PushSymbol (Handle Symbol) (IsA 'Reference)
    deriving stock Generic

kindSymbol :: AffineTraversal' Kind (Handle Symbol)
kindSymbol = singular Generic.types

nodeSymbol :: AffineTraversal' Node (Handle Symbol)
nodeSymbol = _Ctor @"Node" % _1 % kindSymbol

isDefOrRef :: Kind -> Bool
isDefOrRef = \case
  PopScoped _ x -> x ^. bridged
  PopSymbol _ x -> x ^. bridged
  PushScoped _ _ x -> x ^. bridged
  PushSymbol _ x -> x ^. bridged
  _ -> False

kindTag :: Kind -> CInt
kindTag = \case
  DropScopes -> 0
  ExportedScope -> 1
  InternalScope -> 2
  PopScoped{} -> 4
  PopSymbol{} -> 5
  PushScoped{} -> 6
  PushSymbol{} -> 7

data IsA (t :: Clickable) = Yes | No deriving stock Generic
data Clickable = Reference | Definition

class Bridge t where
  type Native t :: Type
  fromNative :: Native t -> t
  toNative :: t -> Native t

bridged :: Bridge t => Iso' t (Native t)
bridged = iso toNative fromNative

instance Bridge (IsA a) where
  type Native (IsA a) = Bool

  fromNative False = No
  fromNative True = Yes

  toNative Yes = True
  toNative No = False

newtype BridgeException = UnrecognizedNodeKind CInt
  deriving stock (Eq, Show)
  deriving anyclass Exception

instance Bridge Node where
  type Native Node = C.Node
  fromNative (C.Node nk (coerce -> ident) (coerce -> sym) sco click) = do
    case nk of
      0 -> -- DROP_SCOPES
        Node DropScopes ident
      1 -> -- EXPORTED_SCOPE
        Node ExportedScope ident
      2 -> -- INTERNAL_SCOPE
        Node InternalScope ident
      3 -> -- JUMP_TO
        JumpTo
      4 -> -- POP_SCOPED_SYMBOL
        Node (PopScoped sym (fromNative click)) ident
      5 -> -- POP_SYMBOL
        Node (PopSymbol sym (fromNative click)) ident
      6 -> -- PUSH_SCOPED_SYMBOL
        Node (PushScoped sym (coerce sco) (fromNative click)) ident
      7 -> -- PUSH_SYMBOL
        Node (PushSymbol sym (fromNative click)) ident
      8 -> -- ROOT
        Root
      x -> throw (UnrecognizedNodeKind x)
  toNative Root =
    C.Node 8 C.rootNodeId nullHandle C.defaultNodeId False
  toNative JumpTo =
    C.Node 3 C.jumpToNodeId nullHandle C.defaultNodeId False
  toNative (Node kind ident) = do
    let sym :: Handle C.Symbol = kind ^? kindSymbol & fromMaybe nullHandle & coerce
    C.Node (kindTag kind) (coerce ident) sym (coerce ident) (isDefOrRef kind)
