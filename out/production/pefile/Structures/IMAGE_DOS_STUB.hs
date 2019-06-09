module Structures.IMAGE_DOS_STUB where

import Data.Int (Int64)
import Data.Word (Word8)
import Data.Serialize.Get (getBytes)
import Data.ByteString (unpack)

image_dos_stub_offset = 0 :: Int64
image_dos_stub_bytes = 96 :: Int

data IMAGE_DOS_STUB = IMAGE_DOS_STUB {
  bytes :: [] Word8
} deriving (Show)

getDosStub = do
  bytes <- getBytes image_dos_stub_bytes
  return $ IMAGE_DOS_STUB $ unpack bytes
