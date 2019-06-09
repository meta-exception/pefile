{-# LANGUAGE DuplicateRecordFields #-}

module Parser.Primitives where

import System.IO.MMap (mmapFileByteString)
import Data.Word (Word8, Word16)
import Data.Serialize.Get as G (runGet, getBytes, getWord8, getWord16le, getWord32le)
import Data.ByteString (unpack)
import Data.Char (chr)

toCharList xs = map chr (map fromIntegral xs)

char_bytes = 1

data IMAGE_STRING = IMAGE_STRING {
  value :: [] Char,
  bytes :: [] Word8
} deriving (Show)

loadWords8 path offset = do
  bs <- mmapFileByteString path (Just (offset, char_bytes))
  case runGet getWord8 bs of
    Left err -> return []
    Right char -> do
      if char == 0
        then return []
      else do
        chars <- loadWords8 path (offset + fromIntegral char_bytes)
        return (char: chars)

loadString path offset = do
  str <- loadWords8 path offset
  return (IMAGE_STRING (toCharList str) str)

unicode_char_bytes = 2

data IMAGE_UNICODE_STRING = IMAGE_UNICODE_STRING {
  value :: [] Char,
  bytes :: [] Word16
} deriving (Show)

loadWords16le path offset = do
  bs <- mmapFileByteString path (Just (offset, unicode_char_bytes))
  case runGet getWord16le bs of
    Left err -> return []
    Right char -> do
      if char == 0
        then return []
      else do
        chars <- loadWords16le path (offset + fromIntegral unicode_char_bytes)
        return (char: chars)


loadUnicodeString path offset = do
  str <- loadWords16le path offset
  return (IMAGE_UNICODE_STRING (toCharList str) str)

getBytes x = do
  bytes <- G.getBytes x
  return (unpack bytes)

loadBytes path offset size = do
  bs <- mmapFileByteString path (Just (offset, size))
  case runGet (Parser.Primitives.getBytes size) bs of
    Left err -> return []
    Right bytes -> return bytes

getWords8 x = do
  if x == 0
  then return []
  else do
    word <- getWord8
    words <- getWords8 (x-1)
    return (word: words)

getWords16le x = do
  if x == 0
  then return []
  else do
    word <- getWord16le
    words <- getWords16le (x-1)
    return (word: words)

getWords32le x = do
  if x == 0
  then return []
  else do
    word <- getWord32le
    words <- getWords32le (x-1)
    return (word: words)
