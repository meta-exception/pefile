module Structures.IMAGE_SECTION_HEADER where

import Data.Word (Word8, Word16, Word32)
import Data.Serialize.Get (getWord8, getWord16le, getWord32le, getBytes)
import Data.ByteString (unpack)

image_section_header_bytes = 40 :: Int

image_sizeof_short_name = 8

getName = do
  bytes <- getBytes image_sizeof_short_name
  return $ unpack bytes

data Union = Union {
  physicalAddress :: Word32,
  virtualSize :: Word32
} deriving (Show)

getUnion = do
  value <- getWord32le
  return $ Union value value

data IMAGE_SECTION_HEADER = IMAGE_SECTION_HEADER {
  name :: [] Word8,
  misc :: Union,
  virtualAddress :: Word32,
  sizeOfRawData :: Word32,
  pointerToRawData :: Word32,
  pointerToRelocations :: Word32,
  pointerToLinenumbers :: Word32,
  numberOfRelocations :: Word16,
  numberOfLinenumbers :: Word16,
  characteristic :: Word32
} deriving (Show)

getSectionHeader = IMAGE_SECTION_HEADER
  <$> getName
  <*> getUnion
  <*> getWord32le
  <*> getWord32le
  <*> getWord32le
  <*> getWord32le
  <*> getWord32le
  <*> getWord16le
  <*> getWord16le
  <*> getWord32le

getSectionHeaders x = do
  if x == 0
    then return []
  else do
    section <- getSectionHeader
    sections <- getSectionHeaders (x-1)
    return (section: sections)
