module Structures.IMAGE_DATA_DIRECTORY where

import Data.Word (Word32)
import Data.Serialize.Get (getWord32le)

image_data_directory_bytes = 8 :: Int

data IMAGE_DATA_DIRECTORY = IMAGE_DATA_DIRECTORY {
  virtualAddress :: Word32,
  size :: Word32
} deriving (Show)

getDataDirectory = IMAGE_DATA_DIRECTORY
  <$> getWord32le
  <*> getWord32le

getDataDirectories x = do
  if x == 0
  then
    return []
  else do
    dir <- getDataDirectory
    dirs <- getDataDirectories (x-1)
    return (dir: dirs)