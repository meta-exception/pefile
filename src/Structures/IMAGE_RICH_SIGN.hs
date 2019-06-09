module Structures.IMAGE_RICH_SIGN where

import Parser.Primitives
     ( getBytes
     )
import Data.Int (Int64)
import Data.Word (Word8)

image_rich_sign_offset = 0 :: Int64
image_rich_sign_bytes = 96 :: Int

data IMAGE_RICH_SIGN = IMAGE_RICH_SIGN {
  bytes :: [] Word8
} deriving (Show)

getRichSign = do
  bytes <- getBytes image_rich_sign_bytes
  return (IMAGE_RICH_SIGN bytes)
