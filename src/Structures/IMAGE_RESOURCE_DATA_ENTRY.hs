module Structures.IMAGE_RESOURCE_DATA_ENTRY where

import Data.Word (Word32)
import Data.Serialize.Get (getWord32le)

image_resource_data_entry_bytes = 16 :: Int

data IMAGE_RESOURCE_DATA_ENTRY = IMAGE_RESOURCE_DATA_ENTRY {
  offsetToData :: Word32,
  size :: Word32,
  codePage :: Word32,
  reserved :: Word32
} deriving (Show)

getResourceDataEntry = IMAGE_RESOURCE_DATA_ENTRY
  <$> getWord32le
  <*> getWord32le
  <*> getWord32le
  <*> getWord32le
