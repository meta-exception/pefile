module Structures.IMAGE_IMPORT_DESCRIPTOR where

import Data.Word (Word32)
import Data.Serialize.Get (getWord32le)

image_import_descriptor_bytes = 20 :: Int

data Union = Union {
  characteristics :: Word32,
  originalFirstThunk :: Word32
} deriving (Show)

getUnion = do
  v <- getWord32le
  return $ Union v v

data IMAGE_IMPORT_DESCRIPTOR = IMAGE_IMPORT_DESCRIPTOR {
  u :: Union,
  timeDateStamp :: Word32,
  forwarderChain :: Word32,
  name :: Word32,
  firstThunk :: Word32
} deriving (Show)

getImportDescriptor = IMAGE_IMPORT_DESCRIPTOR
  <$> getUnion
  <*> getWord32le
  <*> getWord32le
  <*> getWord32le
  <*> getWord32le
