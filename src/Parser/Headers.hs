module Parser.Headers where

import Structures.IMAGE_DOS_STUB
     ( image_dos_stub_bytes, getDosStub
     )
import Structures.IMAGE_DOS_HEADER
     ( image_dos_header_bytes, getDosHeader
     )
import Structures.IMAGE_RICH_SIGN
     ( image_rich_sign_bytes, getRichSign
     )
import Structures.IMAGE_NT_HEADERS
     ( image_nt_headers64_bytes, getNtHeaders
     )
import Data.Serialize.Get (runGet)
import System.IO.MMap (mmapFileByteString)

loadDosHeader path offset = do
  bs <- mmapFileByteString path (Just (offset, image_dos_header_bytes))
  case runGet getDosHeader bs of
    Left err -> return Nothing
    Right header -> return (Just header)

loadDosStub path offset = do
  bs <- mmapFileByteString path (Just (offset, image_dos_stub_bytes))
  case runGet getDosStub bs of
    Left err -> return Nothing
    Right stub -> return (Just stub)

loadRichSign path offset = do
  bs <- mmapFileByteString path (Just (offset, image_rich_sign_bytes))
  case runGet getRichSign bs of
    Left err -> return Nothing
    Right sign -> return (Just sign)

loadNtHeaders path offset = do
  bs <- mmapFileByteString path (Just (offset, image_nt_headers64_bytes))
  case runGet getNtHeaders bs of
    Left err -> return Nothing
    Right header -> return (Just header)
