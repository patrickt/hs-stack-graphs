{-# LANGUAGE ImportQualifiedPost #-}
module Gen (vector) where

import Data.Vector qualified as V
import Hedgehog
import Hedgehog.Gen qualified as Gen

vector :: MonadGen m => Range Int -> m a -> m (V.Vector a)
vector n g = V.fromList <$> Gen.list n g
