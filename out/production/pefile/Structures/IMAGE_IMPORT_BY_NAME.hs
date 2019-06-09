module Structures.IMAGE_IMPORT_BY_NAME where

import Data.Word (Word8, Word16)
import Data.Serialize.Get (getWord8, getWord16le)

data IMAGE_IMPORT_BY_NAME = IMAGE_IMPORT_BY_NAME {
  hint :: Word16,
  name :: [] Word8
} deriving (Show)

getString = do
  ch <- getWord8
  if ch == 0
    then return []
  else do
    chars <- getString
    return (ch: chars)

getImportByName = IMAGE_IMPORT_BY_NAME
  <$> getWord16le
  <*> getString
