{- |
Module: RuleImplementations

Rule implementations implements a whole lot of helper functions, used to
implement the actual rules in the Rules module.
-}

module RuleImplementations where

import Data.Word (Word8(..))
import Data.Char
import Data.List
import Control.Monad.State (modify, put, get, gets)
import Control.Monad.Reader (ask)
import Control.Monad ((=<<), guard)
import Rule

-- | Toggles a Char's case
toggle :: Char -> Char
toggle w = if isUpper w then toLower w else toUpper w

capitalize, rotateLeft, rotateRight :: String -> String
-- | Capitalizes the first Char and lowers the rest
capitalize = applyAt 0 toUpper . fmap toLower

-- | Rotates a String to the left
rotateLeft [] = []
rotateLeft s  = safeTail s ++ [head s]

-- | Rotates a String to the right
rotateRight [] = []
rotateRight s  = last s : safeInit s

-- | Duplicates the first element of a list
duplicateFirst _ [] = []
duplicateFirst n xs = replicate n (head xs) ++ xs

-- | Duplicates the last element of a list
duplicateLast _ [] = []
duplicateLast n xs = xs ++ (replicate n (last xs))

-- | Applies a function to the element at a specific index of a list
applyAt :: (Integral a) => a -> (b -> b) -> [b] -> [b]
applyAt _ _ []     = []
applyAt 0 f (x:xs) = f x : xs
applyAt n f (x:xs) = x : applyAt (n - 1) f xs

-- | Removes the element at a specified index from a list
remove :: (Integral a) => a -> [b] -> [b]
remove n s | n `fits` genericLength s =
                let (xs, ys) = genericSplitAt n s
                in xs ++ (safeTail ys)
           | otherwise = s

-- | Removes the elements at a specified range from a list
removeRange :: (Integral a) => a -> a -> [b] -> [b]
removeRange n m s | n < m     = repeatRemove n (m - n) s
                  | otherwise = s
    where repeatRemove _ 0 s = s
          repeatRemove x n s = remove x $ repeatRemove x (n - 1) s

-- | Inserts an element at a specified index of a list
insertAt :: (Integral a) => a -> b -> [b] -> [b]
insertAt 0 b xs     = b:xs
insertAt _ _ []     = []
insertAt n b (x:xs) = x : insertAt (n - 1) b xs

-- | Replaces all occurances of an element in a list
replace :: (Eq a) => a -> a -> [a] -> [a]
replace x y s = map rp s
    where rp c | c == x    = y
               | otherwise = c

-- | A helper function for applying a function to the ASCII
--   value of a Char
modifyWord8 :: (Word8 -> Word8) -> Char -> Char
modifyWord8 f = fromWord8 . f . toWord8

-- | Casts a Char to Word8
toWord8 :: Char -> Word8
toWord8 = fromIntegral . ord

-- | Casts a Word8 to Char
fromWord8 :: Word8 -> Char
fromWord8 = chr . fromIntegral

-- | Swaps the elements at the specified indexes of a list
swap :: Int -> Int -> [a] -> [a]
swap n m s | n `fits` length s && m `fits` length s =
                applyAt n (const (s !! m)) . applyAt m (const (s !! n)) $ s
           | otherwise = s

-- | Determines wether something is within a specified []-Range
inRange :: (Ord a) => a -> (a, a) -> Bool
a `inRange` (b, c) = a >= b && a <= c

-- | Determines wether an index "fits" into a length
fits :: (Num a, Ord a) => a -> a -> Bool
a `fits` b = a `inRange` (0, b - 1)

-- | Replaces all occurances of two specified elements with each other
twoWayReplace :: (Eq a) => a -> a -> [a] -> [a]
twoWayReplace x y s = map twrp s
    where twrp c | x == c    = y
                 | y == c    = x
                 | otherwise = c

-- | Replaces the element at a specified index with the element at a specified
--   relative index in a list
snatch :: Int -> Int -> [a] -> [a]
snatch n r s | n `fits` length s && (n + r) `fits` length s =
                applyAt n (const (s !! (n + r))) s
             | otherwise = s

-- | Capitalizes each word in a String
title :: String -> String
title = unwords . fmap capitalize . words

-- | A safe implementation of tail that, instead of failing, returns an empty
--   list if the specified list is empty
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

-- | A safe implementation of init that, instead of failing, returns an empty
--   list if the specified list is empty
safeInit :: [a] -> [a]
safeInit [] = []
safeInit (x:[]) = []
safeInit (x:xs) = x:safeInit xs

-- | Appends the String saved in Memory
appendMem :: Rule ()
appendMem = modify $ \s -> s { result = result s ++ stored s }

-- | Prepends the String saved in Memory
prependMem :: Rule ()
prependMem = modify $ \s -> s { result = stored s ++ result s }

-- | Extracts a specified range of the String saved in Memory and inserts
--   it at a specified index
extractMem :: (Integral a) => a -> a -> a -> Rule ()
extractMem n m i = modify $ \s -> s { result = extract (stored s) (result s) }
    where extract mem s = begin s ++ inserted mem ++ end s
          begin s = genericTake i s
          inserted mem = genericTake m . genericDrop n $ mem
          end s = genericDrop i s

-- | Saves the current result to memory and resets the result
saveMem :: Rule ()
saveMem = do
    s <- get
    original <- ask
    put s { stored = result s, result = original }
