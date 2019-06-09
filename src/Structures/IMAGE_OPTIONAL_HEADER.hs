module Structures.IMAGE_OPTIONAL_HEADER where

import Data.Serialize.Get (getWord8, getWord16le, getWord32le, getWord64le)

import Structures.IMAGE_OPTIONAL_HEADER32
import Structures.IMAGE_OPTIONAL_HEADER64
import Structures.IMAGE_DATA_DIRECTORY (getDataDirectories)

image_numberof_directory_entries = 16

image_directory_entry_export = 0 :: Int -- Таблица экспорта
image_directory_entry_import = 1 :: Int -- Таблица импорта (импортируемые библиотеки и функции)
image_directory_entry_resource = 2 :: Int -- Таблица ресурсов
image_directory_entry_exception = 3 :: Int -- Таблица исключений; https://stackoverflow.com/questions/19808172/struct-runtime-function
image_directory_entry_security = 4 :: Int -- Таблица данных о безопасности
image_directory_entry_basereloc = 5 :: Int -- Таблица перемещаемых элементов
image_directory_entry_debug = 6 :: Int -- Таблица отладочной информации
image_directory_entry_architecture = 7 :: Int -- Данные, определяемые конкретной платформой
image_directory_entry_globalptr = 8 :: Int -- unused
image_directory_entry_tls = 9 :: Int -- Таблица данных локальной памяти потока
image_directory_entry_load_config = 10 :: Int -- Таблица данных конфигурации
image_directory_entry_bound_import = 11 :: Int -- Таблица BOUND-импорта
image_directory_entry_iat = 12 :: Int -- Таблица адресов импортируемых функций
image_directory_entry_delay_import = 13 :: Int -- Таблица "задержанного" импорта
image_directory_entry_com_descriptor = 14 :: Int -- Таблица метаданных .NET

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
