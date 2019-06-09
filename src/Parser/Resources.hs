{-# LANGUAGE DuplicateRecordFields #-}

module Parser.Resources where

import Structures.IMAGE_RESOURCE_DIRECTORY
     ( IMAGE_RESOURCE_DIRECTORY, image_resource_directory_bytes, getResourceDirectory,
       numberOfNamedEntries, numberOfIdEntries
     )
import Structures.IMAGE_RESOURCE_DIRECTORY_ENTRY
     ( IMAGE_RESOURCE_DIRECTORY_ENTRY, image_resource_directory_entry_bytes, getResourceDirectoryEntries,
       u, nameOffset, nameIsString,
       u1, dataIsDirectory, offsetToDirectory, offsetToData
     )
import Structures.IMAGE_RESOURCE_DATA_ENTRY
     ( IMAGE_RESOURCE_DATA_ENTRY, image_resource_data_entry_bytes, getResourceDataEntry
     )
import Parser.Primitives
     ( IMAGE_UNICODE_STRING, loadUnicodeString
     )
import System.IO.MMap (mmapFileByteString)
import Data.Serialize.Get (runGet)
import Data.Maybe (catMaybes)

data RESOURCE_DIRECTORY_ENTRY = RESOURCE_DIRECTORY_ENTRY {
  entry :: IMAGE_RESOURCE_DIRECTORY_ENTRY,
  name :: Maybe IMAGE_UNICODE_STRING
} deriving (Show)

data RESOURCE_DIRECTORY_NODE = Node { nodeEntry :: RESOURCE_DIRECTORY_ENTRY, node :: RESOURCE_DIRECTORY_TREE_NODE }
                             | Leaf { nodeEntry :: RESOURCE_DIRECTORY_ENTRY, leaf :: IMAGE_RESOURCE_DATA_ENTRY }
                                deriving (Show)

data RESOURCE_DIRECTORY_TREE_NODE = RESOURCE_DIRECTORY_TREE_NODE {
  value :: IMAGE_RESOURCE_DIRECTORY,
  children :: [] RESOURCE_DIRECTORY_NODE
} deriving (Show)

loadResourceDirectory path offset = do
  bs <- mmapFileByteString path (Just (offset, image_resource_directory_bytes))
  case runGet getResourceDirectory bs of
    Left err -> return Nothing
    Right resourceDirectory -> return (Just resourceDirectory)

loadResourceDirectoryEntryName path offsetToRoot entry = do
  if nameIsString (u entry)
    then do
      name <- loadUnicodeString path (offsetToRoot + (fromIntegral(nameOffset (u entry))))
      return (RESOURCE_DIRECTORY_ENTRY entry (Just name))
    else return (RESOURCE_DIRECTORY_ENTRY entry Nothing)

loadResourceDirectoryEntries path offsetToRoot offset count = do
  bs <- mmapFileByteString path (Just (offset, image_resource_directory_entry_bytes * count))
  case runGet (getResourceDirectoryEntries count) bs of
    Left err -> return Nothing
    Right entries -> do
      entriesWithName <- mapM (loadResourceDirectoryEntryName path offsetToRoot) entries
      return (Just entriesWithName)

loadResourceDataEntry path offset = do
  bs <- mmapFileByteString path (Just (offset, image_resource_data_entry_bytes))
  case runGet getResourceDataEntry bs of
    Left err -> return Nothing
    Right entry -> return (Just entry)

loadResourceDirectoryNodeChildren path offsetToRoot entryWithName = do
  let _entry = entry entryWithName
  if dataIsDirectory (u1 _entry)
    then do
      let _offsetToDirectory = fromIntegral(offsetToDirectory (u1 _entry))
      node <- loadResourceDirectoryTree path offsetToRoot (offsetToRoot + _offsetToDirectory)
      case node of
        Nothing -> return Nothing
        Just node -> return (Just (Node entryWithName node))
    else do
      let _offsetToData = fromIntegral(offsetToData _entry)
      leaf <- loadResourceDataEntry path (offsetToRoot + _offsetToData)
      case leaf of
        Nothing -> return Nothing
        Just leaf -> return (Just (Leaf entryWithName leaf))

loadResourceDirectoryTree path offsetToRoot offset = do
  dir <- loadResourceDirectory path offset
  case dir of
    Nothing -> return Nothing
    Just dir -> do
      let numberOfEntries = fromIntegral(numberOfNamedEntries dir + numberOfIdEntries dir)
      let offsetToEntries = offset + fromIntegral image_resource_directory_bytes
      entries <- loadResourceDirectoryEntries path offsetToRoot offsetToEntries numberOfEntries
      case entries of
        Nothing -> return (Just (RESOURCE_DIRECTORY_TREE_NODE dir []))
        Just entries -> do
          children <- mapM (loadResourceDirectoryNodeChildren path offsetToRoot) entries
          return (Just (RESOURCE_DIRECTORY_TREE_NODE dir (catMaybes children)))
