module Structures.IMAGE_BASE_RELOCATION where

import Data.Word (Word32)
import Data.Serialize.Get (getWord32le)

image_base_relocation_bytes = 8 :: Int

data IMAGE_BASE_RELOCATION = IMAGE_BASE_RELOCATION {
  virtualAddress :: Word32,
  sizeOfBlock :: Word32
} deriving (Show)

getBaseRelocation = IMAGE_BASE_RELOCATION
  <$> getWord32le
  <*> getWord32le
