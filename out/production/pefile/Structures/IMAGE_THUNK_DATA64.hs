module Structures.IMAGE_THUNK_DATA64 where

import Data.Word (Word64)
import Data.Serialize.Get (getWord64le)
import Data.Bits (testBit)

data IMAGE_THUNK_DATA64 = IMAGE_THUNK_DATA64 {
  forwarderString :: Word64,
  function :: Word64,
  ordinal :: Word64,
  addressOfData :: Word64
}

int64_highest_bit = 63

getThunkData64 = do
  value <- getWord64le
  return $ IMAGE_THUNK_DATA64 value value value value
