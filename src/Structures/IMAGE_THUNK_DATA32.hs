module Structures.IMAGE_THUNK_DATA32 where

import Data.Word (Word32)
import Data.Serialize.Get (getWord32le)
import Data.Bits (testBit, clearBit)

image_thunk_data32_bytes = 4

data IMAGE_THUNK_DATA32 = IMAGE_THUNK_DATA32 {
  forwarderString :: Word32,
  function :: Word32,
  ordinal :: Word32,
  addressOfData :: Word32
} deriving (Show)

int32_highest_bit = 31

getThunkData32 = do
  value <- getWord32le
  if testBit value int32_highest_bit
  then do
    let value = clearBit value int32_highest_bit
    return $ IMAGE_THUNK_DATA32 0 value value value
  else return $ IMAGE_THUNK_DATA32 value value 0 value
