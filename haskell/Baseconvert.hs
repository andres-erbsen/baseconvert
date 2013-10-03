{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
module Baseconvert (baseconvert, base, baseDigits, baseLength, Base) where

import Foreign (allocaBytes)
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString.Char8 as B
import Data.ByteString.Unsafe
import System.IO.Unsafe (unsafePerformIO)
import Data.List (nub)

foreign import ccall unsafe "baseconvert.c baseconvert_targetlen"
     c_baseconvert_targetlen :: CUChar -> CUChar -> CUInt -> CUInt

foreign import ccall unsafe "baseconvert.c baseconvert"
     c_baseconvert :: CUChar -> CString ->
                      CString -> CUInt -> 
                      CUChar -> CString ->
                      CString -> CUInt -> CUInt

newtype Base = Base {baseDigits :: ByteString} deriving (Ord, Eq)
baseLength = B.length . baseDigits

base xs 
 | B.length xs > 256 = error "base must consist of 2 <= n <= 256 chars, got too many"
 | B.length xs < 2   = error "base must consist of 2 <= n <= 256 chars, got too few"
 | B.unpack xs /= (nub $ B.unpack xs) = error "Digits of a base must be unique"
 | otherwise = Base xs

instance Show Base where
  show (Base xs) = "base " ++ show xs

baseconvert :: Base -> Base -> ByteString -> ByteString
baseconvert _ _ "" = error "Convert no digits to some digits?!"
baseconvert (Base inDigits) (Base outDigits) toConvert = unsafePerformIO $
  unsafeUseAsCStringLen inDigits  $ \ (in_digits, inRadix) ->
  unsafeUseAsCStringLen outDigits $ \ (out_digits, outRadix) ->
  unsafeUseAsCStringLen toConvert $ \ (in_chars, inLen) ->
  let
    out_len_limit = c_baseconvert_targetlen in_max_digit out_max_digit in_len
    in_max_digit = fromIntegral $ inRadix - 1
    out_max_digit = fromIntegral$ outRadix - 1
    in_len = fromIntegral inLen
  in
    allocaBytes (fromIntegral out_len_limit) $ \ out_chars -> do
      let out_len = c_baseconvert in_max_digit in_digits in_chars in_len
                                out_max_digit out_digits out_chars out_len_limit
      packCStringLen (out_chars, fromIntegral out_len) -- TODO: unsafe 'd be faster
      


-- main = print $ baseconvert (Base "01") (Base "0123456789ABCDEF") "1111"
