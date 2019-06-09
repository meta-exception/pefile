module Structures.IMAGE_FILE_HEADER where

import Data.Word (Word16, Word32)
import Data.Serialize.Get (getWord16le, getWord32le)

image_file_header_bytes = 20 :: Int

data IMAGE_FILE_HEADER = IMAGE_FILE_HEADER {
  machine :: Word16,
  numberOfSections :: Word16,
  timeDateStamp :: Word32,
  pointerToSymbolTable :: Word32,
  numberOfSymbols :: Word32,
  sizeOfOptionalHeader :: Word16,
  characteristics :: Word16
} deriving (Show)

getFileHeader = IMAGE_FILE_HEADER
  <$> getWord16le
  <*> getWord16le
  <*> getWord32le
  <*> getWord32le
  <*> getWord32le
  <*> getWord16le
  <*> getWord16le
