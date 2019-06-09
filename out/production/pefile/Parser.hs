module Parser
     ( loadFile
     ) where

import Structures.IMAGE_DOS_HEADER
     ( image_dos_header_offset, image_dos_header_bytes, getDosHeader, e_lfarlc, e_lfanew
     )
import Structures.IMAGE_DOS_STUB
     (  image_dos_stub_bytes, getDosStub
     )
import Structures.IMAGE_RICH_SIGN
     (  image_rich_sign_bytes, getRichSign
     )
import Structures.IMAGE_NT_HEADERS
     (  image_nt_headers_bytes, getNtHeaders, signature, fileHeader, optionalHeader
     )
import Structures.IMAGE_FILE_HEADER
     (  numberOfSections
     )
import Structures.IMAGE_OPTIONAL_HEADER
     (  IMAGE_OPTIONAL_HEADER(IMAGE_OPTIONAL_HEADER_32, IMAGE_OPTIONAL_HEADER_64)
     )
import Structures.IMAGE_OPTIONAL_HEADER32
     (  fileAlignment, sectionAlignment, dataDirectory
     )
import Structures.IMAGE_OPTIONAL_HEADER64
     (  fileAlignment, sectionAlignment, dataDirectory
     )
import Structures.IMAGE_SECTION_HEADER
     (  image_section_header_bytes, getSectionHeaders
     )
import Structures
     (  offsetOfSection,
        RESOURCE_DIRECTORY_NODE, RESOURCE_DIRECTORY_ENTRY_NODE, RESOURCE_DIRECTORY_ENTRY_VALUE(Node, Leaf)
     )
import Structures.IMAGE_EXPORT_DIRECTORY
     (  image_export_directory_bytes, getExportDirectory
     )
import Structures.IMAGE_IMPORT_DESCRIPTOR
     (  image_import_descriptor_bytes, getImportDescriptor
     )
import Structures.IMAGE_RESOURCE_DIRECTORY
     (  image_resource_directory_bytes, getResourceDirectory, numberOfNamedEntries, numberOfIdEntries
     )
import Structures.IMAGE_RESOURCE_DIRECTORY_ENTRY
     (  image_resource_directory_entry_bytes, getResourceDirectoryEntries,
        u, nameOffset, nameIsString,
        u1, dataIsDirectory, offsetToDirectory
     )
import Logger
     (  printList
     )

import System.IO.MMap
import Data.Int (Int64)
import Data.Serialize.Get (runGet)
import Data.Either (fromRight)
import Data.Word (Word32)
import Structures.IMAGE_DATA_DIRECTORY (virtualAddress)
import Data.Bits (testBit, finiteBitSize)

--p = "/home/default/VirtualBox VMs/VMBoxes_default_1553972272070_10747/packer-virtualbox-iso-1442644747-disk1.vmdk" :: FilePath
path = "/home/default/Desktop/ZA_institute/4.2/Forensics/samples/4tests/calc.exe" :: FilePath
accessMode = WriteCopy :: Mode

loadFile :: IO ()

toBitList x = map (testBit x) [0..(finiteBitSize x-1)]

loadFile = do
  bs <- mmapFileByteString path (Just (image_dos_header_offset, image_dos_header_bytes))
  let dosHeader = runGet getDosHeader bs
  case dosHeader of
    Left err -> print ("Error while reading" ++ err)
    Right header -> do
      print header
      let lfarlc = fromIntegral(e_lfarlc header)
      let lfanew = fromIntegral(e_lfanew header)
      bs <- mmapFileByteString path (Just (lfarlc, image_dos_stub_bytes))
      let dosStub = runGet getDosStub bs
      case dosStub of
        Left err -> print err
        Right header -> print header
      let image_rich_sign_offset = lfarlc + fromIntegral(image_dos_stub_bytes)
      if (image_rich_sign_offset + fromIntegral(image_rich_sign_bytes) <= lfanew)
        then do
          bs <- mmapFileByteString path (Just (image_rich_sign_offset, image_rich_sign_bytes))
          let richSign = runGet getRichSign bs
          case richSign of
            Left err -> print err
            Right header -> print header
        else print "RICH Singature doesn't detected"
      bs <- mmapFileByteString path (Just (lfanew, image_nt_headers_bytes))
      let ntHeaders = runGet getNtHeaders bs
      case ntHeaders of
        Left err -> print err
        Right header -> do
          print (signature header)
          print (fileHeader header)
          let _numberOfSections = fromIntegral(numberOfSections(fileHeader header)) :: Int
          case optionalHeader header of
            IMAGE_OPTIONAL_HEADER_32 header -> do
              print header
              let fileAlignment = Structures.IMAGE_OPTIONAL_HEADER32.fileAlignment header
              let sectionAlignment = Structures.IMAGE_OPTIONAL_HEADER32.sectionAlignment header
              bs <- mmapFileByteString path (Just (lfanew + fromIntegral(image_nt_headers_bytes), image_section_header_bytes * _numberOfSections))
              let sectionHeaders = runGet (getSectionHeaders _numberOfSections) bs
              case sectionHeaders of
                Left err -> print err
                Right sectionHeaders -> do
                  print sectionHeaders
            IMAGE_OPTIONAL_HEADER_64 header -> do
              print header
              printList (Structures.IMAGE_OPTIONAL_HEADER64.dataDirectory header)
              let fileAlignment = Structures.IMAGE_OPTIONAL_HEADER64.fileAlignment header
              let sectionAlignment = Structures.IMAGE_OPTIONAL_HEADER64.sectionAlignment header
              bs <- mmapFileByteString path (Just (lfanew + fromIntegral(image_nt_headers_bytes), image_section_header_bytes * _numberOfSections))
              let sectionHeaders = runGet (getSectionHeaders _numberOfSections) bs
              case sectionHeaders of
                Left err -> print err
                Right sectionHeaders -> do
                  printList sectionHeaders
                  let dataDirectories = Structures.IMAGE_OPTIONAL_HEADER64.dataDirectory header
                  let pointerToExportDirectory = virtualAddress (dataDirectories !! 0)
                  if pointerToExportDirectory /= 0
                    then do
                      let offsetToExportDirectory = offsetOfSection sectionHeaders sectionAlignment pointerToExportDirectory
                      bs <- mmapFileByteString path (Just (offsetToExportDirectory, image_export_directory_bytes))
                      let exportDirectory = runGet getExportDirectory bs
                      case exportDirectory of
                        Left err -> print err
                        Right dir -> print dir
                    else
                      print "No EXPORT_DIRECTORY"
                  let pointerToImportDescriptor = virtualAddress (dataDirectories !! 1)
                  if pointerToImportDescriptor /= 0
                    then do
                      let offsetToImportDescriptor = offsetOfSection sectionHeaders sectionAlignment pointerToImportDescriptor
                      bs <- mmapFileByteString path (Just (offsetToImportDescriptor, image_import_descriptor_bytes))
                      let importDescriptor = runGet getImportDescriptor bs
                      case importDescriptor of
                        Left err -> print err
                        Right desc -> print desc
                    else print "No IMAGE_IMPORT_DESCRIPTOR"
                  let pointerToResourceDirectory = virtualAddress (dataDirectories !! 2)
                  if pointerToResourceDirectory /= 0
                    then do
                      let offsetToResourceDirectory = offsetOfSection sectionHeaders sectionAlignment pointerToResourceDirectory
                      bs <- mmapFileByteString path (Just (offsetToResourceDirectory, image_resource_directory_bytes))
                      let resourceDirectory = runGet getResourceDirectory bs
                      case resourceDirectory of
                        Left err -> print err
                        Right dir -> do
                          print dir
                          let numberOfDirectoryEntries = fromIntegral(numberOfNamedEntries dir + numberOfIdEntries dir)
                          let offsetToDirectoryEntries = offsetToResourceDirectory + fromIntegral(image_resource_directory_bytes)
                          bs <- mmapFileByteString path (Just (offsetToDirectoryEntries, image_resource_directory_entry_bytes * numberOfDirectoryEntries))
                          let directoryEntries = runGet (getResourceDirectoryEntries numberOfDirectoryEntries) bs
                          case directoryEntries of
                            Left err -> print err
                            Right directoryEntries -> do
                              let x = directoryEntries !! 0
                              printList directoryEntries
                              print ""
                    else print "No IMAGE_RESOURCE_DIR"


loadResourceDirectory offsetToResourceDirectory = do
  bs <- mmapFileByteString path (Just (offsetToResourceDirectory, image_resource_directory_bytes))
  let resourceDirectory = runGet getResourceDirectory bs
  case resourceDirectory of
    Left err -> do
      print err
      return Nothing
    Right resourceDirectory -> do
      print resourceDirectory
      return (Just resourceDirectory)



{-|

loadResourceDirectoryNode offsetToRootResourceDirectory offsetToResourceDirectory = do
  resourceDirectory <- loadResourceDirectory offsetToResourceDirectory
  case resourceDirectory of
    Nothing -> return Nothing
    Just resourceDirectory -> do
      print resourceDirectory
      let resourceDirectoryEntries = loadResourceDirectoryEntries offsetToResourceDirectory resourceDirectory
      let resourceDirectoryEntryNodes = mapM (loadResourceDirectoryEntryNode offsetToRootResourceDirectory) resourceDirectoryEntries
      let node = RESOURCE_DIRECTORY_NODE resourceDirectory resourceDirectoryEntryNodes
      return (Just node)
      --return

loadResourceDirectoryEntries offsetToResourceDirectory resourceDirectory = do
  let numberOfDirectoryEntries = fromIntegral(numberOfNamedEntries resourceDirectory + numberOfIdEntries resourceDirectory)
  let offsetToDirectoryEntries = offsetToResourceDirectory + fromIntegral(image_resource_directory_bytes)
  bs <- mmapFileByteString path (Just (offsetToDirectoryEntries, image_resource_directory_entry_bytes * numberOfDirectoryEntries))
  let directoryEntries = runGet (getResourceDirectoryEntries numberOfDirectoryEntries) bs
  return directoryEntries

loadResourceDirectoryEntryNode offsetToRootResourceDirectory resourceDirectoryEntry = do
  let _nameIsString = nameIsString (u resourceDirectoryEntry)
  let _dataIsDirectory = dataIsDirectory (u1 resourceDirectoryEntry)
  if _dataIsDirectory
    then do
      let offsetToResourceDirectory = offsetToRootResourceDirectory + fromIntegral(offsetToDirectory (u1 resourceDirectoryEntry))
      let node = loadResourceDirectoryNode offsetToRootResourceDirectory offsetToResourceDirectory
      return RESOURCE_DIRECTORY_ENTRY_NODE resourceDirectoryEntry (Node node)
    else do
      --let leaf = load
      return RESOURCE_DIRECTORY_ENTRY_NODE resourceDirectoryEntry (Leaf leaf)

print $ testBit (nameOffset (u x)) 31
print $ toBitList (nameOffset (u x))
print $ length $ toBitList (nameOffset (u x))
-}