-- | Helper methods for operating on LED displays
module Shiny.Shiny where

import Data.Colour.RGBSpace
import Data.Word (Word8)

-- | A single LED color triplet
type LED = RGB Word8

-- | An LED array
type Display = [LED]

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