-- | Helper methods for operating on LED displays
module Shiny.Shiny where

import Data.Colour.RGBSpace
import Data.Word (Word8)

import Text.Printf
import Data.List (intercalate)

-- | A single LED color triplet
type LED = RGB Word8

-- | An LED array
type Display = [LED]

-- | Shows a more human-readable display
showDisplay :: Display -> String
showDisplay d = intercalate "\n" $ zipWith showRgb [0..] d
  where
    indexDigits = floor $ logBase 10 $ fromIntegral $ length d
    indexFormat = "%0" ++ show indexDigits ++ "d: "
    showRgb :: Int -> LED -> String
    showRgb i (RGB r g b) = printf (indexFormat ++ "%02x%02x%02x") i r g b

-- | Generates an empty display of the given length
emptyDisplay :: Int -> Display
emptyDisplay n = replicate n (RGB 0 0 0)

-- | Rotates a list to the left
rotateLeft :: Int -> [a] -> [a]
rotateLeft n l = take (length l) $ drop n $ cycle l

-- | Rotates a lift to the right
rotateRight :: Int -> [a] -> [a]
rotateRight n l = rotateLeft (length l - n) l

-- | Rotates right if n is positive, otherwise rotates left
rotate :: Int -> [a] -> [a]
rotate n l
  | n > 0     = rotateRight n l
  | otherwise = rotateLeft (-n) l