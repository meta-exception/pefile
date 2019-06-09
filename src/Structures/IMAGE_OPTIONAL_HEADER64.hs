module Structures.IMAGE_OPTIONAL_HEADER64 where

import Data.Word (Word8, Word16, Word32, Word64)

import Structures.IMAGE_DATA_DIRECTORY

image_optional_header64_bytes = 240

data IMAGE_OPTIONAL_HEADER64 = IMAGE_OPTIONAL_HEADER64 {
  magic :: Word16,
  majorLinkerVersion :: Word8,
  minorLinkerVersion :: Word8,
  sizeOfCode :: Word32,
  sizeOfInitializedData :: Word32,
  sizeOfUninitializedData :: Word32,
  addressOfEntryPoint :: Word32,
  baseOfCode :: Word32,
  baseOfData :: Word32,
  imageBase :: Word32,
  sectionAlignment :: Word32,
  fileAlignment :: Word32,
  majorOperatingSystemVersion :: Word16,
  minorOperatingSystemVersion :: Word16,
  majorImageVersion :: Word16,
  minorImageVersion :: Word16,
  majorSubsystemVersion :: Word16,
  minorSubsystemVersion :: Word16,
  win32VersionValue :: Word32,
  sizeOfImage :: Word32,
  sizeOfHeaders :: Word32,
  checkSum :: Word32,
  subsystem :: Word16,
  dllCharacteristics :: Word16,
  sizeOfStackReserve :: Word64,
  sizeOfStackCommit :: Word64,
  sizeOfHeapReserve :: Word64,
  sizeOfHeapCommit :: Word64,
  loaderFlags :: Word32,
  numberOfRvaAndSizes :: Word32,
  dataDirectory :: [] IMAGE_DATA_DIRECTORY
} deriving (Show)
