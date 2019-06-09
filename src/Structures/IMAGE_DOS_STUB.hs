module Structures.IMAGE_DOS_STUB where

import Parser.Primitives
     ( getBytes
     )
import Data.Int (Int64)
import Data.Word (Word8)
import Text.Printf (printf)

image_dos_stub_offset = 0 :: Int64
image_dos_stub_bytes = 64 :: Int

data IMAGE_DOS_STUB = IMAGE_DOS_STUB {
  bytes :: [] Word8
} deriving (Show)

getDosStub = do
  bytes <- getBytes image_dos_stub_bytes
  return (IMAGE_DOS_STUB bytes)
