module Structures where

import Structures.IMAGE_SECTION_HEADER
     ( virtualAddress, sizeOfRawData, pointerToRawData
     )
import Parser.Primitives
     ( loadBytes
     )
import Data.List (find, foldl1', group, sort)
import Data.Word (Word8)
import Control.Arrow ((&&&))

--aligmentSectionSize :: Int -> Int -> Int
aligmentSectionSize sizeOfRawData sectionAlignment =
  if sizeOfRawData <= sectionAlignment
  then sectionAlignment
  else sectionAlignment * (ceiling (fromIntegral sizeOfRawData / fromIntegral sectionAlignment))

--rvaOfSection :: Int -> Int -> IMAGE_SECTION_HEADER -> Bool
rvaOfSection rva sectionAlignment sectionHeader = do
  let sectionStart = virtualAddress sectionHeader
  let sectionEnd = sectionStart + aligmentSectionSize (sizeOfRawData sectionHeader) sectionAlignment
  (rva >= sectionStart && rva < sectionEnd)

--offsetOfSection :: [IMAGE_SECTION_HEADER] -> Word32 -> Word32 -> Int64
offsetOfSection sectionHeaders sectionAlignment rva =
  case find (rvaOfSection rva sectionAlignment) sectionHeaders of
    Just header -> fromIntegral(toInteger(rva - virtualAddress header + pointerToRawData header))
    Nothing -> fromIntegral(toInteger(0))

itemFrequencies = map (length) . group . sort

entropy :: [Int] -> Int -> Int -> Double
entropy itemFrequencies totalElements logarithmicBase =
  -(foldl1' (+) $ map (\p -> p * (logBase b p)) probabilities)
    where
      is = map fromIntegral itemFrequencies
      l  = fromIntegral totalElements
      b  = fromIntegral logarithmicBase
      probabilities = map (\i -> i / l) $ is

sectionEntropy path sectionHeader fileAlignment = do
  let dataSize = aligmentSectionSize (sizeOfRawData sectionHeader) fileAlignment
  bytes <- loadBytes path (fromIntegral (pointerToRawData sectionHeader)) (fromIntegral dataSize)
  return (entropy (itemFrequencies bytes) (length bytes) 2)

{-|
--getBytes :: Int -> Get [Word8]
getBytes x = do
  if x == 0
    then return []
  else do
    byte <- getWord8
    bytes <- getBytes (x-1)
    return (byte: bytes)
-}
