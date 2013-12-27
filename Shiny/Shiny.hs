module Data.Shiny  where

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
                
-- | Data type for operating on slices of a list
data ListFocus a b = ListFocus { left :: [a],
                                 middle :: [b],
                                 right :: [a]}                 

instance (Show a, Show b) => Show (ListFocus a b) where
  show (ListFocus l m r) = "Left: " ++ show l
                           ++ ", Middle: " ++ show m
                           ++ ", Right: " ++ show r

instance Functor (ListFocus a) where
  fmap f (ListFocus l m r) = ListFocus l (map f m) r
  
-- | Focuses in on a part of a list
focus :: (Int, Int) -- ^ The start and end indices to focus on
         -> [a] -- ^ The list
         -> ListFocus a a 
focus (start, end) l = ListFocus left mid right
  where
    (left, rest) = splitAt start l
    (mid, right) = splitAt (end - start) rest
    
-- | Takes a focused list and returns the list itself
unfocus :: ListFocus a a -> [a]
unfocus (ListFocus l m r) = l ++ m ++ r

-- | Unfocus, then re-focus on different bounds
refocus :: (Int, Int) -> ListFocus a a -> ListFocus a a
refocus bounds = focus bounds . unfocus

-- | Shifts a focus by the given amount
-- | If the amount is positive, a right shift is performed, otherwise a left shift
shiftFocus :: Int -> ListFocus a a -> ListFocus a a
shiftFocus n foc = refocus (start+n, end+n) foc
  where
    start = length (left foc)
    end = start + length (middle foc)

-- | Shifts a focus to the left by the given amount
shiftFocusLeft :: Int -> ListFocus a a -> ListFocus a a
shiftFocusLeft n = shiftFocus (-n)

-- | Shifts a focus to the right by the given amount
shiftFocusRight :: Int -> ListFocus a a -> ListFocus a a
shiftFocusRight = shiftFocus

-- | Applies a function to a list only on the given bounds
applyOn :: (a -> a) -> (Int, Int) -> [a] -> [a]
applyOn f bounds = unfocus . fmap f . focus bounds