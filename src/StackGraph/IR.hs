{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module StackGraph.IR
  ( StackGraph (..)
  , withStackGraph
  , stackGraphAddSymbols
  , stackGraphSymbols
  , Arena
  , withArena
  , List
  , withList
  , listCount
  , listContents
  , HasVector (..)
  , HasHandle (..)
  ) where

import Data.Int
import Data.Vector qualified as V
import Data.Vector.Storable qualified as VS
import Data.Vector.Storable.Mutable qualified as VSM
import Control.Exception ( assert, bracket )
import Data.Vector.Unboxed qualified as VU
import Data.Vector.Unboxed.Mutable qualified as VUM
import Foreign
import GHC.Records
import StackGraph.Foreign qualified as SG
import Foreign.C
import Data.Coerce
import Data.Kind (Type)
import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Data.ByteString.Unsafe qualified as B
import Data.Monoid
import Data.Foldable (fold)
import Data.String (IsString (fromString))

newtype Symbol = Symbol { unSymbol :: ByteString }
  deriving stock (Eq)
  deriving newtype (Show, Semigroup, Monoid, IsString)

newtype StackGraph = StackGraph { unStackGraph :: SG.StackGraphPtr }

withStackGraph :: (StackGraph -> IO a) -> IO a
withStackGraph = bracket (fmap StackGraph SG.stackGraphNew) (SG.stackGraphFree . unStackGraph)

-- todo: more efficient
stackGraphAddSymbol :: StackGraph -> Symbol -> IO (Handle Symbol)
stackGraphAddSymbol sg s = VS.unsafeHead <$> stackGraphAddSymbols sg (V.singleton s)

stackGraphAddSymbols :: StackGraph -> V.Vector Symbol -> IO (VS.Vector (Handle Symbol))
stackGraphAddSymbols (StackGraph sg) syms = do
  let len = V.length syms
  -- todo: more efficiency here
  let concatted = unSymbol (fold syms)
  let lengths :: VS.Vector CSize = VS.generate len (fromIntegral . B.length . unSymbol . V.unsafeIndex syms)

  B.unsafeUseAsCString concatted $ \str -> do
    VS.unsafeWith lengths $ \lens -> do
      syms :: VSM.IOVector (Handle Symbol) <- VSM.unsafeNew len
      VSM.unsafeWith syms $ \symptr -> do
        SG.stackGraphAddSymbols sg (fromIntegral len) str lens (castPtr symptr)
      VS.freeze syms


stackGraphSymbols :: StackGraph -> IO (V.Vector Symbol)
stackGraphSymbols (StackGraph sg) = do
  syms <- SG.stackGraphSymbols sg
  print syms
  bridgeVector syms

newtype Handle t = Handle Int32
  deriving stock (Show, Eq)
  deriving newtype Storable

class HasArena t where
  type ArenaFor t :: Type
  newArenaFor :: IO (ArenaFor t)
  freeArenaFor :: ArenaFor t -> IO ()

instance HasArena Path where
  type ArenaFor Path = SG.PathArenaPtr
  newArenaFor = SG.pathArenaNew
  freeArenaFor = SG.pathArenaFree

newtype Arena t = Arena { unArena :: ArenaFor t }

withArena :: forall t a . HasArena t => (Arena t -> IO a) -> IO a
withArena f = bracket (newArenaFor @t) (freeArenaFor @t) (f . Arena)

newtype List t = List { unList :: Ptr (ListFor t) }

class HasList t where
  type ListFor t :: Type
  newList :: IO (List t)
  freeList :: List t -> IO ()
  listGetCount :: List t -> IO CSize
  listGetContents :: List t -> IO (Ptr t)

withList :: forall t a . HasList t => (List t -> IO a) -> IO a
withList = bracket (newList @t) (freeList @t)

listCount :: HasList t => List t -> IO Int
listCount = fmap fromIntegral . listGetCount

listContents :: (HasList t, Storable t) => List t -> IO (V.Vector t)
listContents t = do
  len <- listGetCount t
  ptr <- listGetContents t
  V.generateM (fromIntegral len) (\n -> peek (ptr `plusPtr` (n * sizeOf (undefined :: SG.SymbolStackCell))))


newtype Path = Path SG.Path
  deriving newtype Storable

instance HasList Path where
  type ListFor Path = SG.PathList
  newList = fmap List SG.pathListNew
  freeList = SG.pathListFree . unList
  listGetCount = SG.pathListCount . unList
  listGetContents = coerce . SG.pathListPaths . unList

newtype PartialPath = PartialPath SG.PartialPath
  deriving newtype Storable

instance HasList PartialPath where
  type ListFor PartialPath = SG.PartialPathList
  newList = fmap List SG.partialPathListNew
  freeList = SG.partialPathListFree . unList
  listGetCount = SG.partialPathListCount . unList
  listGetContents = coerce . SG.partialPathListPaths . unList

class HasHandle t where
  bridgeHandle :: Int32 -> Handle t
  bridgeHandle = Handle

instance HasHandle SG.Node

instance HasHandle SG.Symbol

instance HasHandle SG.File

instance HasHandle SG.ScopeStackCell

instance HasHandle SG.SymbolStackCell

instance HasHandle SG.PathEdgeListCell

instance HasHandle SG.PartialScopeStackCell

instance HasHandle SG.PartialSymbolStackCell

instance HasHandle SG.PartialPathEdgeListCell

instance HasHandle SG.PartialPath

class HasVector t cells | t -> cells where
  bridgeVector :: cells -> IO (V.Vector t)

deref :: SG.SymbolStackCellPtr -> Int -> IO SG.SymbolStackCell
deref p x = peekByteOff p (x * sizeOf (undefined :: SG.SymbolStack))

convCells ::
  forall a t.
  ( HasField "count" a CSize,
    HasField "cells" a (Ptr t),
    Storable t, Storable a
  ) =>
  a ->
  IO (V.Vector t)
convCells item = do
  let cells = getField @"cells" item
  let count = getField @"count" item
  V.generateM (fromIntegral count) (\n -> peek (cells `plusPtr` (n * sizeOf (undefined :: SG.SymbolStackCell))))

instance HasVector SG.SymbolStackCell SG.SymbolStackCells where bridgeVector = convCells
instance HasVector SG.ScopeStackCell SG.ScopeStackCells where bridgeVector = convCells
instance HasVector SG.PathEdgeListCell SG.PathEdgeListCells where bridgeVector = convCells
instance HasVector SG.PartialSymbolStackCell SG.PartialSymbolStackCells where bridgeVector = convCells

instance HasVector Symbol SG.Symbols where
  bridgeVector item = do
    let count = getField @"count" item
    let syms = getField @"symbols" item
    print item
    V.generateM (fromIntegral count - 1) $ \n -> do
      curr :: SG.Symbol <- peekByteOff syms (n * sizeOf (undefined :: SG.Symbol))
      let siz = assert (fromIntegral @_ @Int (SG.symbol_len curr) >= 0) (fromIntegral (SG.symbol_len curr))
      print siz
      if siz == 0
        then pure mempty
        else Symbol <$> B.packCStringLen (getField @"symbol" curr, siz)
