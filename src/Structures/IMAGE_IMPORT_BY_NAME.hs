module Structures.IMAGE_IMPORT_BY_NAME where

import Data.Word (Word8, Word16)

data IMAGE_IMPORT_BY_NAME = IMAGE_IMPORT_BY_NAME {
  hint :: Word16,
  name :: [] Word8
} deriving (Show)
