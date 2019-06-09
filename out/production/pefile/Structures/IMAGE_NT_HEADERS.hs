module Structures.IMAGE_NT_HEADERS where

import Data.Word (Word32)
import Data.Serialize.Get (getWord32le)

import Structures.IMAGE_FILE_HEADER
import Structures.IMAGE_OPTIONAL_HEADER

-- In case of 64-bit binary
image_nt_headers_bytes = 264 :: Int

data IMAGE_NT_HEADERS = IMAGE_NT_HEADERS {
  signature :: Word32,
  fileHeader :: IMAGE_FILE_HEADER,
  optionalHeader :: IMAGE_OPTIONAL_HEADER
} deriving (Show)

getNtHeaders = IMAGE_NT_HEADERS
  <$> getWord32le
  <*> getFileHeader
  <*> getOptionalHeader
