module Parser.Exports where

import Structures.IMAGE_EXPORT_DIRECTORY
     ( IMAGE_EXPORT_DIRECTORY, image_export_directory_bytes, getExportDirectory,
       name, addressOfFunctions, addressOfNames, addressOfNameOrdinals, numberOfFunctions, numberOfNames
     )
import Structures
     ( offsetOfSection
     )
import Parser.Primitives
     ( IMAGE_STRING(value), loadString, getWords32le, getWords16le
     )
import Data.Word (Word32, Word16)
import Data.Serialize.Get (runGet, getWord32le)
import System.IO.MMap (mmapFileByteString)

loadExportDirectory path offset = do
  bs <- mmapFileByteString path (Just (offset, image_export_directory_bytes))
  let exportDirectory = runGet getExportDirectory bs
  case exportDirectory of
    Left err -> return Nothing
    Right dir -> return (Just dir)

rva_bytes = 4

loadFunctions path sectionHeaders sectionAlignment directory = do
  let offset = offsetOfSection sectionHeaders sectionAlignment (addressOfFunctions directory)
  let count = fromIntegral (numberOfFunctions directory)
  bs <- mmapFileByteString path (Just (offset, count * rva_bytes))
  case runGet (getWords32le count) bs of
    Left err -> return []
    Right functions -> do
      return functions

loadName path sectionHeaders sectionAlignment rva = do
  name <- loadString path (offsetOfSection sectionHeaders sectionAlignment rva)
  return (value name)

loadNames path sectionHeaders sectionAlignment directory = do
  let namesOffset = offsetOfSection sectionHeaders sectionAlignment (addressOfNames directory)
  let namesCount = fromIntegral (numberOfNames directory)
  bs <- mmapFileByteString path (Just (namesOffset, namesCount * rva_bytes))
  case runGet (getWords32le namesCount) bs of
    Left err -> return []
    Right namesRVA -> do
      names <- mapM (loadName path sectionHeaders sectionAlignment) namesRVA
      return names

loadRVAs path sectionHeaders sectionAlignment directory = do
  let rvaOffset = offsetOfSection sectionHeaders sectionAlignment (addressOfFunctions directory)
  let rvaCount = fromIntegral (numberOfFunctions directory)
  bs <- mmapFileByteString path (Just (rvaOffset, rvaCount * rva_bytes))
  case runGet (getWords32le rvaCount) bs of
    Left err -> return []
    Right rva -> return rva

ordinal_bytes = 2

loadExportTable path sectionHeaders sectionAlignment directory = do
  let ordinalsOffset = offsetOfSection sectionHeaders sectionAlignment (addressOfNameOrdinals directory)
  let namesCount = fromIntegral (numberOfNames directory)
  bs <- mmapFileByteString path (Just (ordinalsOffset, namesCount * ordinal_bytes))
  case runGet (getWords16le namesCount) bs of
    Left err -> return []
    Right ordinals -> do
      _RVAs <- loadRVAs path sectionHeaders sectionAlignment directory
      names <- loadNames path sectionHeaders sectionAlignment directory
      return (zipWith3 EXPORT_ENTRY ordinals _RVAs names)

data EXPORT_ENTRY = EXPORT_ENTRY {
  ordinal :: Word16,
  rva :: Word32,
  functionName :: [] Char
} deriving (Show)

data EXPORT_DIRECTORY = EXPORT_DIRECTORY {
  directory :: IMAGE_EXPORT_DIRECTORY,
  dllName :: IMAGE_STRING,
  exportTable :: [EXPORT_ENTRY]
} deriving (Show)

loadExports path sectionHeaders sectionAlignment virtualAddress = do
  if virtualAddress > 0
  then do
    directory <- loadExportDirectory path (offsetOfSection sectionHeaders sectionAlignment virtualAddress)
    case directory of
      Nothing -> return Nothing
      Just directory -> do
        dllName <- loadString path (offsetOfSection sectionHeaders sectionAlignment (name directory))
        exportTable <- loadExportTable path sectionHeaders sectionAlignment directory
        return (Just (EXPORT_DIRECTORY directory dllName exportTable))
  else return Nothing