-- | Functions for performing operations on a subsection of a list
module Shiny.Focus (
  Focus,
  -- * Creating a focus
  onIndices,
  range,
  slice,
  -- * Transforming focuses
  shiftRight,
  shiftLeft,
  shift,
  alsoOn,
  exceptOn,
  -- * Focus utilities
  between,
  apply,
  replace,
  focusOn
  ) where

import Data.List ((\\))

-- | Represents a subsection of a list
newtype Focus = Focus {
  indices :: [Int]
  } deriving (Show)

-- | Constructs a Focus using the indices that satisfy a condition
onIndices :: (Int -> Bool) -> Focus
onIndices pred = Focus (filter pred [0..])

-- | Focuses on the list between the first bound, inclusive, and the second, exclusive
-- | This is equivalent to onIndices
range :: Int -> Int -> Focus
range start end = Focus [start..end-1]

-- | Emulation of python slices.
slice :: Int -> -- ^ The start, inclusive
         Int -> -- ^ The end, inclusive
         Int -> -- ^ The step between indices
         Focus
slice start end step = Focus [start,start+step..end-1]

-- | Shifts each index in a focus to the right by n
shiftRight :: Int -> Focus -> Focus
shiftRight n (Focus i) = Focus (map (+n) i)

-- | Shifts each index in a focus to the left by n
shiftLeft :: Int -> Focus -> Focus
shiftLeft n = shiftRight (-n)

-- | Shifts each index in a focus to the right if n is positive, otherwise to the left
shift :: Int -> Focus -> Focus
shift = shiftRight

-- | Combines two focuses together
alsoOn :: Focus -> Focus -> Focus
alsoOn (Focus a) (Focus b) = Focus (a ++ b)

-- | Removes the second focus from the first
exceptOn :: Focus -> Focus -> Focus
exceptOn (Focus a) (Focus b) = Focus (a \\ b)

-- | Creates a predicate on indices between two indices
between :: Int    -- ^ The start, inclusive
           -> Int -- ^ The end, inclusive
           -> Int -- ^ The index
           -> Bool
between start end i = start <= i && i < end

-- | Applies a function to the elements in focus
apply :: (a -> a) -> Focus -> [a] -> [a]
apply func (Focus ilist) = zipWith indexFunc [0..]
  where
    indexFunc i x = if i `elem` ilist
                    then func x
                    else x

-- | Replaces the focused elements
replace :: Focus  -- ^ The focus
           -> [a] -- ^ What to replace the focused elements with
           -> [a] -- ^ The original list
           -> [a] -- ^ The new list
replace (Focus ilist) rlist = foldl (.) id repFuncs -- Form a replacement function for each relevant index, and compose them
  where
    repFuncs = zipWith rep ilist rlist
    rep i r xs = let (left, (_:right)) = splitAt i xs
                 in left ++ [r] ++ right

-- | Returns only the elements of the list focused on
focusOn ::Focus-> [a] -> [a]
focusOn (Focus ilist) = map snd . filter ((`elem` ilist) . fst) . zip [0..]