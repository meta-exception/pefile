module Structures.IMAGE_DOS_HEADER where

import Parser.Primitives
     ( getWords16le
     )
import Data.Int (Int64)
import Data.Word (Word16, Word32)
import Data.Serialize.Get (getWord16le, getWord32le)

image_dos_header_offset = 0 :: Int64
image_dos_header_bytes = 64 :: Int

e_res_length = 4
e_res2_length = 10

data IMAGE_DOS_HEADER = IMAGE_DOS_HEADER {
  e_magic :: Word16,
  e_cblp :: Word16,
  e_cp :: Word16,
  e_crlc :: Word16,
  e_cparhdr :: Word16,
  e_minalloc :: Word16,
  e_maxalloc :: Word16,
  e_ss :: Word16,
  e_sp :: Word16,
  e_csum :: Word16,
  e_ip :: Word16,
  e_cs :: Word16,
  e_lfarlc :: Word16,
  e_ovno :: Word16,
  e_res :: [] Word16,
  e_oemid :: Word16,
  e_oeminfo :: Word16,
  e_res2 :: [] Word16,
  e_lfanew :: Word32
} deriving (Show)

-- getDosHeader :: Get IMAGE_DOS_HEADER
getDosHeader = IMAGE_DOS_HEADER
  <$> getWord16le
  <*> getWord16le
  <*> getWord16le
  <*> getWord16le
  <*> getWord16le
  <*> getWord16le
  <*> getWord16le
  <*> getWord16le
  <*> getWord16le
  <*> getWord16le
  <*> getWord16le
  <*> getWord16le
  <*> getWord16le
  <*> getWord16le
  <*> getWords16le e_res_length
  <*> getWord16le
  <*> getWord16le
  <*> getWords16le e_res2_length
  <*> getWord32le
