module Structures where

import Structures.IMAGE_SECTION_HEADER
     ( IMAGE_SECTION_HEADER, virtualAddress, sizeOfRawData, pointerToRawData
     )
import Structures.IMAGE_RESOURCE_DIRECTORY
     ( IMAGE_RESOURCE_DIRECTORY
     )
import Structures.IMAGE_RESOURCE_DIRECTORY_ENTRY
     ( IMAGE_RESOURCE_DIRECTORY_ENTRY
     )
import Structures.IMAGE_RESOURCE_DATA_ENTRY
     (  IMAGE_RESOURCE_DATA_ENTRY
     )
import Data.List (find)

--aligmentSectionSize :: Int -> Int -> Int
aligmentSectionSize sizeOfRawData sectionAlignment = do
  if sizeOfRawData <= sectionAlignment
    then sectionAlignment
    else sectionAlignment * (ceiling (fromIntegral sizeOfRawData / fromIntegral sectionAlignment))

--rvaOfSection :: Int -> Int -> IMAGE_SECTION_HEADER -> Bool
rvaOfSection rva sectionAlignment sectionHeader = do
  let sectionStart = virtualAddress sectionHeader
  let sectionEnd = sectionStart + aligmentSectionSize (sizeOfRawData sectionHeader) sectionAlignment
  (rva >= sectionStart && rva < sectionEnd)

--offsetOfSection :: [IMAGE_SECTION_HEADER] -> Word32 -> Word32 -> Int64
offsetOfSection sectionHeaders sectionAlignment rva =
  case find (rvaOfSection rva sectionAlignment) sectionHeaders of
    Just header -> fromIntegral(toInteger(rva - virtualAddress header + pointerToRawData header))
    Nothing -> fromIntegral(toInteger(0))

data RESOURCE_DIRECTORY_NODE = RESOURCE_DIRECTORY_NODE {
  value :: IMAGE_RESOURCE_DIRECTORY,
  children :: [] RESOURCE_DIRECTORY_ENTRY_NODE
} deriving (Show)

data RESOURCE_DIRECTORY_ENTRY_NODE = RESOURCE_DIRECTORY_ENTRY_NODE {
  entry :: IMAGE_RESOURCE_DIRECTORY_ENTRY,
  node :: RESOURCE_DIRECTORY_ENTRY_VALUE
} deriving (Show)

-- Node IMAGE_RESOURCE_DIRECTORY
data RESOURCE_DIRECTORY_ENTRY_VALUE = Node RESOURCE_DIRECTORY_NODE | Leaf IMAGE_RESOURCE_DATA_ENTRY
  deriving (Show)


{-|
--getBytes :: Int -> Get [Word8]
getBytes x = do
  if x == 0
    then return []
  else do
    byte <- getWord8
    bytes <- getBytes (x-1)
    return (byte: bytes)
-}
