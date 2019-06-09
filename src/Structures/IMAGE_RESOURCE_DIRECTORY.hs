module Structures.IMAGE_RESOURCE_DIRECTORY where

import Data.Word (Word16, Word32)
import Data.Serialize.Get (getWord16le, getWord32le)

image_resource_directory_bytes = 16 :: Int

data IMAGE_RESOURCE_DIRECTORY = IMAGE_RESOURCE_DIRECTORY {
  characteristics :: Word32,
  timeDateStamp :: Word32,
  majorVersion :: Word16,
  minorVersion :: Word16,
  numberOfNamedEntries :: Word16,
  numberOfIdEntries :: Word16
} deriving (Show)

getResourceDirectory = IMAGE_RESOURCE_DIRECTORY
  <$> getWord32le
  <*> getWord32le
  <*> getWord16le
  <*> getWord16le
  <*> getWord16le
  <*> getWord16le
