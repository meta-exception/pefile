module Structures.IMAGE_OPTIONAL_HEADER where

import Data.Serialize.Get (getWord8, getWord16le, getWord32le, getWord64le)

import Structures.IMAGE_OPTIONAL_HEADER32
import Structures.IMAGE_OPTIONAL_HEADER64
import Structures.IMAGE_DATA_DIRECTORY (getDataDirectories)

image_numberof_directory_entries = 16;

data IMAGE_OPTIONAL_HEADER = IMAGE_OPTIONAL_HEADER_32 IMAGE_OPTIONAL_HEADER32 | IMAGE_OPTIONAL_HEADER_64 IMAGE_OPTIONAL_HEADER64
  deriving (Show)

getOptionalHeader = do
  magic <- getWord16le
  majorLinkerVersion <- getWord8
  minorLinkerVersion <- getWord8
  sizeOfCode <- getWord32le
  sizeOfInitializedData <- getWord32le
  sizeOfUninitializedData <- getWord32le
  addressOfEntryPoint <- getWord32le
  baseOfCode <- getWord32le
  baseOfData <- getWord32le
  imageBase <- getWord32le
  sectionAlignment <- getWord32le
  fileAlignment <- getWord32le
  majorOperatingSystemVersion  <- getWord16le
  minorOperatingSystemVersion  <- getWord16le
  majorImageVersion  <- getWord16le
  minorImageVersion  <- getWord16le
  majorSubsystemVersion  <- getWord16le
  minorSubsystemVersion  <- getWord16le
  win32VersionValue <- getWord32le
  sizeOfImage <- getWord32le
  sizeOfHeaders <- getWord32le
  checkSum <- getWord32le
  subsystem  <- getWord16le
  dllCharacteristics  <- getWord16le
  case magic of
    0x10b -> do
      sizeOfStackReserve <- getWord32le
      sizeOfStackCommit <- getWord32le
      sizeOfHeapReserve <- getWord32le
      sizeOfHeapCommit <- getWord32le
      loaderFlags <- getWord32le
      numberOfRvaAndSizes <- getWord32le
      dataDirectory <- getDataDirectories image_numberof_directory_entries

      return $! IMAGE_OPTIONAL_HEADER_32 $! IMAGE_OPTIONAL_HEADER32
        magic
        majorLinkerVersion
        minorLinkerVersion
        sizeOfCode
        sizeOfInitializedData
        sizeOfUninitializedData
        addressOfEntryPoint
        baseOfCode
        baseOfData
        imageBase
        sectionAlignment
        fileAlignment
        majorOperatingSystemVersion
        minorOperatingSystemVersion
        majorImageVersion
        minorImageVersion
        majorSubsystemVersion
        minorSubsystemVersion
        win32VersionValue
        sizeOfImage
        sizeOfHeaders
        checkSum
        subsystem
        dllCharacteristics
        sizeOfStackReserve
        sizeOfStackCommit
        sizeOfHeapReserve
        sizeOfHeapCommit
        loaderFlags
        numberOfRvaAndSizes
        dataDirectory
    0x20b -> do
      sizeOfStackReserve <- getWord64le
      sizeOfStackCommit <- getWord64le
      sizeOfHeapReserve <- getWord64le
      sizeOfHeapCommit <- getWord64le
      loaderFlags <- getWord32le
      numberOfRvaAndSizes <- getWord32le
      dataDirectory <- getDataDirectories image_numberof_directory_entries

      return $! IMAGE_OPTIONAL_HEADER_64 $! IMAGE_OPTIONAL_HEADER64
        magic
        majorLinkerVersion
        minorLinkerVersion
        sizeOfCode
        sizeOfInitializedData
        sizeOfUninitializedData
        addressOfEntryPoint
        baseOfCode
        baseOfData
        imageBase
        sectionAlignment
        fileAlignment
        majorOperatingSystemVersion
        minorOperatingSystemVersion
        majorImageVersion
        minorImageVersion
        majorSubsystemVersion
        minorSubsystemVersion
        win32VersionValue
        sizeOfImage
        sizeOfHeaders
        checkSum
        subsystem
        dllCharacteristics
        sizeOfStackReserve
        sizeOfStackCommit
        sizeOfHeapReserve
        sizeOfHeapCommit
        loaderFlags
        numberOfRvaAndSizes
        dataDirectory
