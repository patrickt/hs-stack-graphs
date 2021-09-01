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
  ( StackGraph
  , withStackGraph
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
import Data.Vector.Storable (Vector)
import Data.Vector.Storable qualified as V
import Data.Vector.Storable.Mutable qualified as VM
import Foreign
import GHC.Records
import StackGraph.Foreign qualified as SG
import Foreign.C
import Data.Coerce
import Control.Exception (bracket)
import Data.Kind (Type)

newtype StackGraph = StackGraph { unStackGraph :: SG.StackGraphPtr }

withStackGraph :: (StackGraph -> IO a) -> IO a
withStackGraph = bracket (fmap StackGraph SG.stackGraphNew) (SG.stackGraphFree . unStackGraph)

newtype Handle t = Handle Int32

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

listContents :: (Storable t, HasList t) => List t -> IO (Vector t)
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
  bridgeVector :: cells -> IO (Vector t)

deref :: SG.SymbolStackCellPtr -> Int -> IO SG.SymbolStackCell
deref p x = peekByteOff p (x * sizeOf (undefined :: SG.SymbolStack))

convCells ::
  forall a t.
  ( HasField "count" a CSize,
    HasField "cells" a (Ptr t),
    Storable t, Storable a
  ) =>
  a ->
  IO (Vector t)
convCells item = do
  let cells = getField @"cells" item
  let count = getField @"count" item
  V.generateM (fromIntegral count) (\n -> peek (cells `plusPtr` (n * sizeOf (undefined :: SG.SymbolStackCell))))

instance HasVector SG.SymbolStackCell SG.SymbolStackCells where bridgeVector = convCells
instance HasVector SG.ScopeStackCell SG.ScopeStackCells where bridgeVector = convCells
instance HasVector SG.PathEdgeListCell SG.PathEdgeListCells where bridgeVector = convCells
instance HasVector SG.PartialSymbolStackCell SG.PartialSymbolStackCells where bridgeVector = convCells
