module Structures.IMAGE_THUNK_DATA32 where

import Data.Word (Word32)
import Data.Serialize.Get (getWord32le)
import Data.Bits (testBit)

data IMAGE_THUNK_DATA32 = IMAGE_THUNK_DATA32 {
  forwarderString :: Word32,
  function :: Word32,
  ordinal :: Word32,
  addressOfData :: Word32
}

int32_highest_bit = 31

getThunkData32 = do
  value <- getWord32le
  let s = testBit value int32_highest_bit
  if s
    then return $ IMAGE_THUNK_DATA32 value value value value
    else return $ IMAGE_THUNK_DATA32 value value value value
