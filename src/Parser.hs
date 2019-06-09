module Parser
     ( loadFile
     ) where

import Parser.Headers
     ( loadDosHeader, loadDosStub, loadRichSign, loadNtHeaders
     )
import Structures.IMAGE_DOS_HEADER
     ( image_dos_header_offset, e_lfarlc, e_lfanew
     )
import Structures.IMAGE_DOS_STUB
     ( image_dos_stub_bytes
     )
import Structures.IMAGE_RICH_SIGN
     ( image_rich_sign_bytes
     )
import Structures.IMAGE_NT_HEADERS
     ( image_nt_headers64_bytes, fileHeader, optionalHeader, image_nt_headers32_bytes
     )
import Structures.IMAGE_FILE_HEADER
     ( numberOfSections
     )
import Structures.IMAGE_OPTIONAL_HEADER
     ( IMAGE_OPTIONAL_HEADER(IMAGE_OPTIONAL_HEADER_32, IMAGE_OPTIONAL_HEADER_64),
       image_directory_entry_export, image_directory_entry_import, image_directory_entry_resource, image_directory_entry_basereloc,
     )
import Structures.IMAGE_OPTIONAL_HEADER32
     ( fileAlignment, sectionAlignment, dataDirectory
     )
import Structures.IMAGE_OPTIONAL_HEADER64
     ( fileAlignment, sectionAlignment, dataDirectory
     )
import Signatures
     ( checkSectionsUPX
     )
import Parser.Sections
     ( loadSectionHeaders, section
     )
import Structures
     ( offsetOfSection
     )
import Parser.Exports
     ( loadExports
     )
import Parser.Imports
     ( loadImports32, loadImports64
     )
import Parser.Resources
     ( loadResourceDirectoryTree
     )
import Parser.Relocations
     ( loadBaseRelocations
     )
import Text.Pretty.Simple (pPrint)

import System.IO.MMap
import Data.Int (Int64)
import Data.Serialize.Get (runGet)
import Data.Either (fromRight)
import Data.Word (Word32)
import Structures.IMAGE_DATA_DIRECTORY (virtualAddress)
import Data.Bits (testBit, finiteBitSize)
import Data.Maybe (catMaybes)
import Data.Char (chr)
import System.Environment (getArgs)

--p = "/home/default/VirtualBox VMs/VMBoxes_default_1553972272070_10747/packer-virtualbox-iso-1442644747-disk1.vmdk" :: FilePath
path = "/home/default/Desktop/ZA_institute/4.2/Computer Forensics/samples/4tests/calc.exe" :: FilePath
accessMode = WriteCopy :: Mode

toBitList x = map (testBit x) [0..(finiteBitSize x-1)]

loadFile :: IO ()
loadFile = do
  args <- getArgs
  let path = head args
  dosHeader <- loadDosHeader path image_dos_header_offset
  case dosHeader of
    Nothing -> pPrint "Error while reading DOS Header"
    Just header -> do
      pPrint header
      let lfarlc = fromIntegral(e_lfarlc header)
      let lfanew = fromIntegral(e_lfanew header)
      dosStub <- loadDosStub path lfarlc
      case dosStub of
        Nothing -> pPrint ""
        Just header -> pPrint header
      let image_rich_sign_offset = lfarlc + fromIntegral(image_dos_stub_bytes)
      if (image_rich_sign_offset + fromIntegral(image_rich_sign_bytes) <= lfanew)
        then do
          richSign <- loadRichSign path image_rich_sign_offset
          case richSign of
            Nothing -> pPrint ""
            Just header -> pPrint header
        else pPrint "File doesn't contain RICH Signature"
      ntHeaders <- loadNtHeaders path lfanew
      case ntHeaders of
        Nothing -> pPrint ""
        Just header -> do
          pPrint header
          let _numberOfSections = fromIntegral(numberOfSections(fileHeader header)) :: Int
          case optionalHeader header of
            IMAGE_OPTIONAL_HEADER_32 header -> do
              let fileAlignment = Structures.IMAGE_OPTIONAL_HEADER32.fileAlignment header
              let sectionAlignment = Structures.IMAGE_OPTIONAL_HEADER32.sectionAlignment header
              sectionHeaders <- loadSectionHeaders path fileAlignment (lfanew + fromIntegral(image_nt_headers32_bytes)) _numberOfSections
              case sectionHeaders of
                [] -> pPrint ""
                sectionHeaders -> do
                  pPrint sectionHeaders
                  let _sectionHeaders = map section sectionHeaders
                  if checkSectionsUPX _sectionHeaders
                    then pPrint "DETECTED UPX"
                    else pPrint ""
                  let dataDirectories = Structures.IMAGE_OPTIONAL_HEADER32.dataDirectory header
                  exports <- loadExports path _sectionHeaders sectionAlignment (virtualAddress (dataDirectories !! image_directory_entry_export))
                  pPrint exports
                  imports <- loadImports32 path _sectionHeaders sectionAlignment (virtualAddress (dataDirectories !! image_directory_entry_import))
                  pPrint imports
                  let pointerToResourceDirectory = virtualAddress (dataDirectories !! image_directory_entry_resource)
                  if pointerToResourceDirectory /= 0
                    then do
                      let offsetToResourceDirectory = offsetOfSection _sectionHeaders sectionAlignment pointerToResourceDirectory
                      resourceDirectoryTree <- loadResourceDirectoryTree path offsetToResourceDirectory offsetToResourceDirectory
                      case resourceDirectoryTree of
                        Nothing -> pPrint ""
                        Just dir -> pPrint dir
                    else pPrint "File doesn't contain IMAGE_RESOURCE_DIRECTORY"
                  let pointerToBaseRelocation = virtualAddress (dataDirectories !! image_directory_entry_basereloc)
                  if pointerToBaseRelocation /= 0
                    then do
                      let offsetToBaseRelocation = offsetOfSection _sectionHeaders sectionAlignment pointerToBaseRelocation
                      basereloc <- loadBaseRelocations path offsetToBaseRelocation
                      pPrint basereloc
                    else pPrint "File doesn't contain IMAGE_BASE_RELOCATION"
            IMAGE_OPTIONAL_HEADER_64 header -> do
              let fileAlignment = Structures.IMAGE_OPTIONAL_HEADER64.fileAlignment header
              let sectionAlignment = Structures.IMAGE_OPTIONAL_HEADER64.sectionAlignment header
              sectionHeaders <- loadSectionHeaders path fileAlignment (lfanew + fromIntegral(image_nt_headers64_bytes)) _numberOfSections
              case sectionHeaders of
                [] -> pPrint ""
                sectionHeaders -> do
                  pPrint sectionHeaders
                  let _sectionHeaders = map section sectionHeaders
                  if checkSectionsUPX _sectionHeaders
                    then pPrint "DETECTED UPX"
                    else pPrint ""
                  let dataDirectories = Structures.IMAGE_OPTIONAL_HEADER64.dataDirectory header
                  exports <- loadExports path _sectionHeaders sectionAlignment (virtualAddress (dataDirectories !! image_directory_entry_export))
                  pPrint exports
                  imports <- loadImports64 path _sectionHeaders sectionAlignment (virtualAddress (dataDirectories !! image_directory_entry_import))
                  pPrint imports
                  let pointerToResourceDirectory = virtualAddress (dataDirectories !! image_directory_entry_resource)
                  if pointerToResourceDirectory /= 0
                    then do
                      let offsetToResourceDirectory = offsetOfSection _sectionHeaders sectionAlignment pointerToResourceDirectory
                      resourceDirectoryTree <- loadResourceDirectoryTree path offsetToResourceDirectory offsetToResourceDirectory
                      case resourceDirectoryTree of
                        Nothing -> pPrint ""
                        Just dir -> pPrint dir
                    else pPrint "File doesn't contain IMAGE_RESOURCE_DIRECTORY"
                  let pointerToBaseRelocation = virtualAddress (dataDirectories !! image_directory_entry_basereloc)
                  if pointerToBaseRelocation /= 0
                    then do
                      let offsetToBaseRelocation = offsetOfSection _sectionHeaders sectionAlignment pointerToBaseRelocation
                      basereloc <- loadBaseRelocations path offsetToBaseRelocation
                      pPrint basereloc
                    else pPrint "File doesn't contain IMAGE_BASE_RELOCATION"