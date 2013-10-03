{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Word
import Baseconvert
import Control.Monad

import Test.QuickCheck

shuffle :: (Eq a) => [a] -> Gen [a]
shuffle [] = return []
shuffle xs = do
  x <- oneof $ map return xs
  ys <- shuffle . filter (/= x) $ xs
  return (x:ys)

bytes :: ByteString
bytes = B.pack [0x00 .. 0xFF]

instance Arbitrary Base where
  arbitrary = do
    r <- arbitrary :: Gen Int
    let n = 2 + (r `mod` 255) -- random integer 2..256
    liftM (base . B.pack . take n) . shuffle . B.unpack $ bytes

prop_convertback :: Base -> Base -> NonEmptyList Int -> Bool
prop_convertback baseA baseB (NonEmpty for_digitValues) =
  let
    digitValues = map (`mod` (baseLength baseA)) $ for_digitValues
    digits = B.pack . map (B.index (baseDigits baseA)) $ digitValues
    digits' = baseconvert baseB baseA . baseconvert baseA baseB $ digits
    zeroA = B.head . baseDigits $ baseA
    zeroB = B.head . baseDigits $ baseB
  in if B.all (==zeroA) digits
       then digits' == B.pack [zeroA]
       else digits' == (B.dropWhile (==zeroA) digits)

main = quickCheck prop_convertback
-- do
--  let digitDecs = [0]
--  let baseA = base "01"
--  let baseB = base bytes
--  let a = baseLength baseA
--  let digits = B.pack . map (B.index (baseDigits baseA) . (`mod` a)) $ digitDecs
--  print $ digits
--  print $ baseconvert baseA baseB $ digits
--  print $ (baseconvert baseB baseA . baseconvert baseA baseB) $ digits
  
