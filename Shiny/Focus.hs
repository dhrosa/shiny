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
  unfocus
  ) where

-- | Represents a subsection of a list
data Focus a = Focus {
  indices :: [Int],
  list :: [a]
  } deriving (Show)

-- | Constructs a Focus using the indices that satisfy a condition
onIndices :: (Int -> Bool) -> [a] -> Focus a
onIndices pred l = Focus (filter pred [0..length l - 1]) l

-- | Focuses on the list between the first bound, inclusive, and the second, exclusive
-- | This is equivalent to onIndices
range :: Int -> Int -> [a] -> Focus a
range start end = Focus [start..end-1]

-- | Emulation of python slices.
slice :: (Int, Int, Int) -- ^ The inclusive start, exclusive start, and the step between indices
         -> [a] -- ^ The list to focus on
         -> Focus a
slice (start, end, step) = Focus [start,start+step..end-1]

-- | Shifts each index in a focus to the right by n
shiftRight :: Int -> Focus a -> Focus a
shiftRight n (Focus i l) = Focus (map (+n) i) l

-- | Shifts each index in a focus to the left by n
shiftLeft :: Int -> Focus a -> Focus a
shiftLeft n = shiftRight (-n)

-- | Shifts each index in a focus to the right if n is positive, otherwise to the left
shift :: Int -> Focus a -> Focus a
shift = shiftRight

-- | Adds indices satisfying the predicates
alsoOn :: (Int -> Bool) -> Focus a -> Focus a
alsoOn pred (Focus i l) = Focus (i ++ filter pred [0..length l - 1]) l

-- | Removes the indices satisfying the predicate
exceptOn :: (Int -> Bool) -> Focus a -> Focus a
exceptOn pred (Focus i l) = Focus (filter (not.pred) i) l

-- | Creates a predicate on indices between two indices
between :: Int    -- ^ The start, inclusive
           -> Int -- ^ The end, inclusive
           -> Int -- ^ The index
           -> Bool
between start end i = start <= i && i < end

-- | Applies a function to the elements in focus
apply :: (a -> a) -> Focus a -> Focus a
apply func (Focus ilist list) = Focus ilist newList
  where
    newList = zipWith indexFunc [0..] list
    indexFunc i x = if i `elem` ilist
                    then func x
                    else x
                         
-- | Converts a focus back into a list
unfocus :: Focus a -> [a]
unfocus (Focus _ l) = l