module RuleImplementations where

import Data.Word (Word8(..))
import Data.Char
import Data.List
import Rule

toggle :: Char -> Char
toggle w = if isUpper w then toLower w else toUpper w

capitalize, rotateLeft, rotateRight :: String -> String
capitalize = applyAt 0 toUpper . fmap toLower

rotateLeft [] = []
rotateLeft s  = safeTail s ++ [head s]

rotateRight [] = []
rotateRight s  = last s : safeInit s

duplicateFirst _ [] = []
duplicateFirst n xs = replicate n (head xs) ++ xs

duplicateLast _ [] = []
duplicateLast n xs = xs ++ (replicate n (last xs))

applyAt :: (Integral a) => a -> (b -> b) -> [b] -> [b]
applyAt _ _ []     = []
applyAt 0 f (x:xs) = f x : xs
applyAt n f (x:xs) = x : applyAt (n - 1) f xs

remove :: (Integral a) => a -> [b] -> [b]
remove n s | n `fits` genericLength s =
                let (xs, ys) = genericSplitAt n s
                in xs ++ (safeTail ys)
           | otherwise = s

removeRange :: (Integral a) => a -> a -> [b] -> [b]
removeRange n m s | n < m     = repeatRemove n (m - n) s
                  | otherwise = s
    where repeatRemove _ 0 s = s
          repeatRemove x n s = remove x $ repeatRemove x (n - 1) s

insertAt :: (Integral a) => a -> b -> [b] -> [b]
insertAt 0 b xs     = b:xs
insertAt _ _ []     = []
insertAt n b (x:xs) = x : insertAt (n - 1) b xs

replace :: (Eq a) => a -> a -> [a] -> [a]
replace x y s = map rp s
    where rp c | c == x    = y
               | otherwise = c

modifyWord8 :: (Word8 -> Word8) -> Char -> Char
modifyWord8 f = fromWord8 . f . toWord8

toWord8 :: Char -> Word8
toWord8 = fromIntegral . ord

fromWord8 :: Word8 -> Char
fromWord8 = chr . fromIntegral

swap :: Int -> Int -> [a] -> [a]
swap n m s | n `fits` length s && m `fits` length s =
                applyAt n (const (s !! m)) . applyAt m (const (s !! n)) $ s
           | otherwise = s

inRange :: (Ord a) => a -> (a, a) -> Bool
a `inRange` (b, c) = a >= b && a <= c

fits :: (Num a, Ord a) => a -> a -> Bool
a `fits` b = a `inRange` (0, b - 1)

twoWayReplace :: (Eq a) => a -> a -> [a] -> [a]
twoWayReplace x y s = map twrp s
    where twrp c | x == c    = y
                 | y == c    = x
                 | otherwise = c

snatch :: Int -> Int -> [a] -> [a]
snatch n r s | n `fits` length s && (n + r) `fits` length s =
                applyAt n (const (s !! (n + r))) s
             | otherwise = s

title :: String -> String
title = intercalate " " . fmap capitalize . words

safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

safeInit :: [a] -> [a]
safeInit [] = []
safeInit (x:[]) = []
safeInit (x:xs) = x:safeInit xs

appendMem :: Rule
appendMem = liftMemR $ \(a, mem) -> (a ++ stored mem, mem)

prependMem :: Rule
prependMem = liftMemR $ \(a, mem) -> (stored mem ++ a, mem)

extractMem :: (Integral a) => a -> a -> a -> Rule
extractMem n m i = liftMemR $ \(a, mem) -> (extract (stored mem) a, mem)
    where extract mem s = begin s ++ inserted mem ++ end s
          begin s = genericTake i s
          inserted mem = genericTake m . genericDrop n $ mem
          end s = genericDrop i s
