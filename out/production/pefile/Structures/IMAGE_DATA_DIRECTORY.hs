module Structures.IMAGE_DATA_DIRECTORY where

import Data.Word (Word32)
import Data.Serialize.Get (getWord32le)

image_data_directory_bytes = 64

data IMAGE_DATA_DIRECTORY = IMAGE_DATA_DIRECTORY {
  virtualAddress :: Word32,
  size :: Word32
} --deriving (Show)

instance Show IMAGE_DATA_DIRECTORY where
  show (IMAGE_DATA_DIRECTORY virtualAddress size) =
    "[IMAGE_DATA_DIRECTORY]\n" ++
    padList "VirtualAddress:\t" 20 ' ' ++ show virtualAddress ++ "\n" ++
    "Size:          \t" ++ show size

getDataDirectory = IMAGE_DATA_DIRECTORY
  <$> getWord32le
  <*> getWord32le

getDataDirectories x = do
  if x == 0
    then return []
  else do
    dir <- getDataDirectory
    dirs <- getDataDirectories (x-1)
    return (dir: dirs)