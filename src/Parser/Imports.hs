{-# LANGUAGE DuplicateRecordFields #-}

module Parser.Imports where

import Structures.IMAGE_IMPORT_DESCRIPTOR
     ( IMAGE_IMPORT_DESCRIPTOR, image_import_descriptor_bytes, getImportDescriptor,
       u, characteristics, originalFirstThunk,
       timeDateStamp, forwarderChain, name, firstThunk
     )
import Structures.IMAGE_THUNK_DATA64 as Thunk64
     ( IMAGE_THUNK_DATA64(IMAGE_THUNK_DATA64), image_thunk_data64_bytes, getThunkData64, forwarderString, function, ordinal, addressOfData
     )
import Structures.IMAGE_THUNK_DATA32 as Thunk32
     ( IMAGE_THUNK_DATA32(IMAGE_THUNK_DATA32), image_thunk_data32_bytes, getThunkData32, forwarderString, function, ordinal, addressOfData
     )
import Structures.IMAGE_IMPORT_BY_NAME
     ( IMAGE_IMPORT_BY_NAME(IMAGE_IMPORT_BY_NAME)
     )
import Structures
     ( offsetOfSection
     )
import Parser.Primitives
     ( IMAGE_STRING(bytes, value), loadString
     )
import Data.Serialize.Get (runGet, getWord16le)
import System.IO.MMap (mmapFileByteString)

loadImportDescriptor path offset = do
  bs <- mmapFileByteString path (Just (offset, image_import_descriptor_bytes))
  case runGet getImportDescriptor bs of
    Left err -> return Nothing
    Right desc -> return (Just desc)

loadImportDescriptors path offset = do
  desc <- loadImportDescriptor path offset
  case desc of
    Nothing -> return []
    Just desc -> do
      if characteristics (u desc) /= 0
        || originalFirstThunk (u desc) /= 0
        || timeDateStamp desc /= 0
        || forwarderChain desc /= 0
        || name desc /= 0
        || firstThunk desc /= 0
      then do
        descriptors <- loadImportDescriptors path (offset + fromIntegral image_import_descriptor_bytes)
        return (desc: descriptors)
      else return []

data IMPORT_BY_NAME = IMPORT_BY_NAME {
  struct :: IMAGE_IMPORT_BY_NAME,
  functionName :: [] Char
} deriving (Show)

data THUNK_DATA64 = THUNK_DATA64 {
  thunk :: IMAGE_THUNK_DATA64,
  importNameTable :: Maybe IMPORT_BY_NAME
} deriving (Show)

data THUNK_DATA32 = THUNK_DATA32 {
  thunk :: IMAGE_THUNK_DATA32,
  importNameTable :: Maybe IMPORT_BY_NAME
} deriving (Show)

data IMPORT_DESCRIPTOR64 = IMPORT_DESCRIPTOR64 {
  descriptor :: IMAGE_IMPORT_DESCRIPTOR,
  dllName :: [] Char,
  importLookupTable :: [] THUNK_DATA64,
  importAddressTable :: [] THUNK_DATA64
} deriving (Show)

data IMPORT_DESCRIPTOR32 = IMPORT_DESCRIPTOR32 {
  descriptor :: IMAGE_IMPORT_DESCRIPTOR,
  dllName :: [] Char,
  importLookupTable :: [] THUNK_DATA32,
  importAddressTable :: [] THUNK_DATA32
} deriving (Show)

size_of_hint = 2

loadImportByName path offset = do
  if offset > 0
  then do
    bs <- mmapFileByteString path (Just (offset, size_of_hint))
    case runGet getWord16le bs of
      Left err -> return Nothing
      Right hint -> do
        name <- loadString path (offset + fromIntegral size_of_hint)
        return (Just (IMPORT_BY_NAME (IMAGE_IMPORT_BY_NAME hint (bytes name)) (value name)))
  else return Nothing

loadThunkData64 path sectionHeaders sectionAlignment offset = do
  bs <- mmapFileByteString path (Just (offset, fromIntegral image_thunk_data64_bytes))
  case runGet getThunkData64 bs of
    Left err -> return []
    Right thunk -> do
      if Thunk64.forwarderString thunk /= 0
        || Thunk64.function thunk /= 0
        || Thunk64.ordinal thunk /= 0
        || Thunk64.addressOfData thunk /= 0
      then do
        importNameTable <- loadImportByName path (offsetOfSection sectionHeaders sectionAlignment (fromIntegral (Thunk64.forwarderString thunk)))
        thunks <- loadThunkData64 path sectionHeaders sectionAlignment (offset + fromIntegral image_thunk_data64_bytes)
        return ((THUNK_DATA64 thunk importNameTable): thunks)
      else return []

loadThunkData32 path sectionHeaders sectionAlignment offset = do
  bs <- mmapFileByteString path (Just (offset, fromIntegral image_thunk_data32_bytes))
  case runGet getThunkData32 bs of
    Left err -> return []
    Right thunk -> do
      if Thunk32.forwarderString thunk /= 0
        || Thunk32.function thunk /= 0
        || Thunk32.ordinal thunk /= 0
        || Thunk32.addressOfData thunk /= 0
      then do
        importNameTable <- loadImportByName path (offsetOfSection sectionHeaders sectionAlignment (fromIntegral (Thunk32.forwarderString thunk)))
        thunks <- loadThunkData32 path sectionHeaders sectionAlignment (offset + fromIntegral image_thunk_data32_bytes)
        return ((THUNK_DATA32 thunk importNameTable): thunks)
      else return []


loadImportTable64 path sectionHeaders sectionAlignment desc = do
  importLookupTable <- loadThunkData64 path sectionHeaders sectionAlignment (offsetOfSection sectionHeaders sectionAlignment (originalFirstThunk (u desc)))
  importAddressTable <- loadThunkData64 path sectionHeaders sectionAlignment ((offsetOfSection sectionHeaders sectionAlignment (firstThunk desc)))
  dllName <- loadString path (offsetOfSection sectionHeaders sectionAlignment (name desc))
  return (IMPORT_DESCRIPTOR64 desc (value dllName) importLookupTable importAddressTable)

loadImportTable32 path sectionHeaders sectionAlignment desc = do
  importLookupTable <- loadThunkData32 path sectionHeaders sectionAlignment (offsetOfSection sectionHeaders sectionAlignment (originalFirstThunk (u desc)))
  importAddressTable <- loadThunkData32 path sectionHeaders sectionAlignment ((offsetOfSection sectionHeaders sectionAlignment (firstThunk desc)))
  dllName <- loadString path (offsetOfSection sectionHeaders sectionAlignment (name desc))
  return (IMPORT_DESCRIPTOR32 desc (value dllName) importLookupTable importAddressTable)

loadImports64 path sectionHeaders sectionAlignment virtualAddress = do
  if virtualAddress > 0
  then do
    descriptors <- loadImportDescriptors path (offsetOfSection sectionHeaders sectionAlignment virtualAddress)
    tables <- mapM (loadImportTable64 path sectionHeaders sectionAlignment) descriptors
    return (Just tables)
  else return Nothing

loadImports32 path sectionHeaders sectionAlignment virtualAddress = do
  if virtualAddress > 0
  then do
    descriptors <- loadImportDescriptors path (offsetOfSection sectionHeaders sectionAlignment virtualAddress)
    tables <- mapM (loadImportTable32 path sectionHeaders sectionAlignment) descriptors
    return (Just tables)
  else return Nothing
