module Structures.IMAGE_EXPORT_DIRECTORY where

import Data.Word (Word16, Word32)
import Data.Serialize.Get (getWord16le, getWord32le)

image_export_directory_bytes = 0 :: Int

data IMAGE_EXPORT_DIRECTORY = IMAGE_EXPORT_DIRECTORY {
  characteristics :: Word32,
  timeDateStamp :: Word32,
  majorVersion :: Word16,
  minorVersion :: Word16,
  name :: Word32,
  base :: Word32,
  numberOfFunctions :: Word32,
  numberOfNames :: Word32,
  addressOfFunctions :: Word32,
  addressOfNames :: Word32,
  addressOfNameOrdinals :: Word32
} deriving (Show)

getExportDirectory = IMAGE_EXPORT_DIRECTORY
  <$> getWord32le
  <*> getWord32le
  <*> getWord16le
  <*> getWord16le
  <*> getWord32le
  <*> getWord32le
  <*> getWord32le
  <*> getWord32le
  <*> getWord32le
  <*> getWord32le
  <*> getWord32le
