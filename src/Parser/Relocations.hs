module Parser.Relocations where

import Structures.IMAGE_BASE_RELOCATION
     ( IMAGE_BASE_RELOCATION, image_base_relocation_bytes, getBaseRelocation, sizeOfBlock, virtualAddress
     )
import Parser.Primitives
     ( getWords8
     )
import Data.Word (Word8)
import System.IO.MMap (mmapFileByteString)
import Data.Serialize.Get (runGet, getWord8, isEmpty)

data TYPE_OFFSET = TYPE_OFFSET {
  _type :: Word8,
  offset :: Word8
} deriving (Show)

getTypeOffset = TYPE_OFFSET
  <$> getWord8
  <*> getWord8

getTypeOffsets = do
  empty <- isEmpty
  if empty
    then return []
    else do
      offset <- getTypeOffset
      offsets <- getTypeOffsets
      return (offset: offsets)

data BASE_RELOCATION = BASE_RELOCATION {
  basereloc :: IMAGE_BASE_RELOCATION,
  typeOffsets :: [] TYPE_OFFSET
} deriving (Show)

loadBaseRelocation path offset = do
  bs <- mmapFileByteString path (Just (offset, image_base_relocation_bytes))
  case runGet getBaseRelocation bs of
    Left err -> return Nothing
    Right relocation -> do
      let typeOffsetsCount = fromIntegral(sizeOfBlock relocation) - fromIntegral(image_base_relocation_bytes)
      if typeOffsetsCount > 0
        then do
          bs <- mmapFileByteString path (Just (offset + fromIntegral(image_base_relocation_bytes), typeOffsetsCount))
          case runGet getTypeOffsets bs of
            Left err -> return (Just (BASE_RELOCATION relocation []))
            Right typeOffsets -> return (Just (BASE_RELOCATION relocation typeOffsets))
        else return Nothing

loadBaseRelocations path offset = do
  relocation <- loadBaseRelocation path offset
  case relocation of
    Nothing -> return []
    Just relocation -> do
      let _basereloc = basereloc relocation
      if virtualAddress _basereloc /= 0
        then do
          relocs <- loadBaseRelocations path (offset + fromIntegral(sizeOfBlock _basereloc))
          return (relocation: relocs)
        else return [relocation]
