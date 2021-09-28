module StackGraph.Variable (Variable (..)) where

import Foreign.Storable
import Data.Word
import GHC.Generics (Generic)

newtype Variable t = Variable {unVariable :: Word32}
  deriving stock (Eq, Show, Generic)
  deriving newtype (Storable)
