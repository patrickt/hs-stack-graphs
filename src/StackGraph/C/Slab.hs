{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- todo: rename to slab

module StackGraph.C.Slab
  ( Slab,
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
import GHC.Stack (HasCallStack)
import StackGraph.Handle
import Prelude hiding (length, lookup)

data Slab a = Slab {buffer :: Ptr a, count :: CSize}
  deriving stock (Eq, Show, Generic)
  deriving anyclass (GStorable)

length :: Slab a -> Int
length = fromIntegral . count

lookup :: (HasCallStack, Storable a) => Handle a -> Slab a -> IO a
lookup (Handle h) (Slab buffer count) = do
  when (h > fromIntegral count) (throw (HandleIndexTooLarge h))
  peek (advancePtr buffer (fromIntegral h))

unsafeLookup :: Storable a => Handle a -> Slab a -> IO a
unsafeLookup (Handle h) (Slab buffer _) = peek (advancePtr buffer (fromIntegral h))
{-# INLINE unsafeLookup #-}

newtype SlabException
  = HandleIndexTooLarge Word32
  deriving stock (Show, Eq)
  deriving anyclass (Exception)

contents :: forall a. Storable a => Slab a -> IO [a]
contents (Slab buf siz) = peekArray (fromIntegral siz) buf
{-# INLINE contents #-}

contents' :: forall a. Storable a => Slab a -> IO (VS.Vector a)
contents' (Slab buf siz) = do
  let n = fromIntegral siz * sizeOf @a undefined
  fp <- mallocPlainForeignPtrBytes n
  withForeignPtr fp $ \p -> copyBytes p buf n
  return $ VS.unsafeFromForeignPtr0 fp (fromIntegral siz)
