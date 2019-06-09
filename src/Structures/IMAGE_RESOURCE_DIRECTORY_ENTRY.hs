module Structures.IMAGE_RESOURCE_DIRECTORY_ENTRY where

import Data.Word (Word16, Word32)
import Data.Serialize.Get (getWord32le)
import Data.Bits (testBit, clearBit, (.&.))

image_resource_directory_entry_bytes = 8 :: Int

data Union = Union {
  nameOffset :: Word32,
  nameIsString :: Bool,
  _pad :: Word32,
  id :: Word32
} deriving (Show)

data Union1 = Union1 {
  offsetToDirectory :: Word32,
  dataIsDirectory :: Bool
} deriving (Show)

data IMAGE_RESOURCE_DIRECTORY_ENTRY = IMAGE_RESOURCE_DIRECTORY_ENTRY {
  name :: Word32,
  u :: Union,
  offsetToData :: Word32,
  u1 :: Union1
} deriving (Show)

int32_highest_bit = 31

getResourceDirectoryEntry = do
  name <- getWord32le
  let nameOffset = clearBit name int32_highest_bit
  let nameIsString = testBit name int32_highest_bit
  let _pad = name .&. 0xFFFF0000
  let id = name .&. 0x0000FFFF
  offsetToData <- getWord32le
  let offsetToDirectory = clearBit offsetToData int32_highest_bit
  let dataIsDirectory = testBit offsetToData int32_highest_bit
  return $ IMAGE_RESOURCE_DIRECTORY_ENTRY name (Union nameOffset nameIsString _pad id) offsetToData (Union1 offsetToDirectory dataIsDirectory)

getResourceDirectoryEntries x = do
  if x == 0
  then
    return []
  else do
    dir <- getResourceDirectoryEntry
    dirs <- getResourceDirectoryEntries (x-1)
    return (dir: dirs)
