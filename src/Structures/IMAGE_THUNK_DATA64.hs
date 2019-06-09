module Structures.IMAGE_THUNK_DATA64 where

import Data.Word (Word64)
import Data.Serialize.Get (getWord64le)
import Data.Bits (testBit, clearBit)

image_thunk_data64_bytes = 8

data IMAGE_THUNK_DATA64 = IMAGE_THUNK_DATA64 {
  forwarderString :: Word64,
  function :: Word64,
  ordinal :: Word64,
  addressOfData :: Word64
} deriving (Show)

int64_highest_bit = 63

getThunkData64 = do
  value <- getWord64le
  if testBit value int64_highest_bit
  then do
    let value = clearBit value int64_highest_bit
    return $ IMAGE_THUNK_DATA64 0 value value value
  else return $ IMAGE_THUNK_DATA64 value value 0 value
