{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module StackGraph.C.Arena
  ( Arena,
    length,
    lookup,
    unsafeLookup,
    contents,
    contents',
  )
where

import Control.Exception
import Control.Monad
import Data.Vector.Storable qualified as VS
import Foreign
import Foreign.C
import Foreign.Storable.Generic (GStorable)
import GHC.ForeignPtr
import GHC.Generics (Generic)
import StackGraph.Handle
import Prelude hiding (length, lookup)

data Arena a = Arena {buffer :: Ptr a, count :: CSize}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GStorable)

length :: Arena a -> Int
length = fromIntegral . count

lookup, unsafeLookup :: Storable a => Handle a -> Arena a -> IO a
lookup (Handle h) (Arena buffer count) = do
  when (h > fromIntegral count) (throw (HandleIndexTooLarge h))
  peek (advancePtr buffer (fromIntegral h))
unsafeLookup (Handle h) (Arena buffer _) = peek (advancePtr buffer (fromIntegral h))

newtype ArenaException
  = HandleIndexTooLarge Word32
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

contents :: forall a. Storable a => Arena a -> IO [a]
contents (Arena buf siz) = peekArray (fromIntegral siz) buf
{-# INLINE contents #-}

contents' :: forall a. Storable a => Arena a -> IO (VS.Vector a)
contents' (Arena buf siz) = do
  let n = fromIntegral siz * sizeOf @a undefined
  fp <- mallocPlainForeignPtrBytes n
  withForeignPtr fp $ \p -> copyBytes p buf n
  return $ VS.unsafeFromForeignPtr0 fp (fromIntegral siz)
