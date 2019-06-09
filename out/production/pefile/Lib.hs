module Lib
    (
    ) where

import System.IO.MMap
import Data.Int (Int64)
import Data.Serialize.Get (runGet)
import Data.ByteString
import Data.List (find)
import Data.Maybe (isJust, maybe)
import Text.Read (readMaybe)
import Data.Either (rights, fromRight, isRight, fromLeft)

path = "/home/default/Desktop/ZA_institute/4.2/Forensics/samples/4tests/calc.exe" :: FilePath

accessMode = WriteCopy :: Mode


p = "/home/default/VirtualBox VMs/VMBoxes_default_1553972272070_10747/packer-virtualbox-iso-1442644747-disk1.vmdk" :: FilePath

--  :: [(ptr, offset, size)] -> prt
-- getForeignByOffset ((a,b):xs) =

matchOffset x (ptr, offset, size) = x <= offset + size && x >= offset

--getTupleByOffset x xs = find (matchOffset x) xs

first (x, _, _) = x
--getForeignByOffset x xs = maybe undefined first $ getTupleByOffset x xs


{-|
loadFile = do
  (ptr, size, offset) <- mmapFileForeignPtr p accessMode chunk
  print (ptr, size, offset)
  print "Heh"


loadFile = do
  str <- mmapFileByteString path chunk
  let h = runGet getDosHeader (fromStrict str)
  print h
  s <- mmapFileForeignPtrLazy p accessMode Nothing
  let g = getForeignByOffset 13107201000 s
  print g
  print $ length s
-}