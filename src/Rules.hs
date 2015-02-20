module Rules
    ( Rule(..)
    , parseRule
    ) where

import Data.Word (Word8(..))
import Data.Char
import Data.List
import Data.Bits (shift)

type Rule = String -> String

parseRule :: String -> Rule
parseRule []       = id
parseRule (' ':rs) = parseRule rs

-- | Argumentless functions
parseRule (':':rs) = parseRule rs . id
parseRule ('l':rs) = parseRule rs . fmap toLower
parseRule ('u':rs) = parseRule rs . fmap toUpper
parseRule ('t':rs) = parseRule rs . fmap toggle
parseRule ('c':rs) = parseRule rs . capitalize
parseRule ('r':rs) = parseRule rs . reverse
parseRule ('d':rs) = parseRule rs . (\s -> s ++ s)
parseRule ('f':rs) = parseRule rs . (\s -> s ++ reverse s)
parseRule ('{':rs) = parseRule rs . rotateLeft
parseRule ('}':rs) = parseRule rs . rotateRight
parseRule ('[':rs) = parseRule rs . safeTail
parseRule (']':rs) = parseRule rs . safeInit
parseRule ('q':rs) = parseRule rs . (\s -> concat . map (replicate 2) $ s)
parseRule ('C':rs) = parseRule ('c':'t':rs)
parseRule ('k':rs) = parseRule rs . swap 0 1
parseRule ('K':rs) = parseRule rs . (\s -> swap (length s - 2) (length s - 1) s)
parseRule ('E':rs) = parseRule rs . title

-- | One argument functions
parseRule ('T':n:rs)  = parseRule rs . applyAt (toDigit n) toggle
parseRule ('p':n:rs)  = parseRule rs . concat . replicate (toDigit n)
parseRule ('D':n:rs)  = parseRule rs . remove (toDigit n)
parseRule ('z':n:rs)  = parseRule rs . duplicateFirst (toDigit n)
parseRule ('Z':n:rs)  = parseRule rs . duplicateLast (toDigit n)
parseRule ('\'':n:rs) = parseRule rs . take (toDigit n)
parseRule ('@':c:rs)  = parseRule rs . (\s -> filter (== c) s)
parseRule ('$':c:rs)  = parseRule rs . (\s -> s ++ [c])
parseRule ('^':c:rs)  = parseRule rs . (c :)
parseRule ('R':n:rs)  = parseRule rs . applyAt (toDigit n) (\c -> fromWord8 (toWord8 c `shift` (-1)))
parseRule ('L':n:rs)  = parseRule rs . applyAt (toDigit n) (\c -> fromWord8 (toWord8 c `shift` 1))
parseRule ('+':n:rs)  = parseRule rs . applyAt (toDigit n) (\c -> fromWord8 (toWord8 c + 1))
parseRule ('-':n:rs)  = parseRule rs . applyAt (toDigit n) (\c -> fromWord8 (toWord8 c - 1))
parseRule ('.':n:rs)  = parseRule rs . snatch (toDigit n) 1
parseRule (',':n:rs)  = parseRule rs . snatch (toDigit n) (-1)
parseRule ('y':n:rs)  = parseRule rs . (\s -> (take (toDigit n) s) ++ s)
parseRule ('Y':n:rs)  = parseRule rs . (\s -> s ++ (reverse . take (toDigit n) . reverse $ s))

-- | Two argument functions
parseRule ('x':n:m:rs) = parseRule rs . removeRange (toDigit n) (toDigit m)
parseRule ('i':n:c:rs) = parseRule rs . insertAt (toDigit n) (c)
parseRule ('o':n:c:rs) = parseRule rs . applyAt (toDigit n) (const c)
parseRule ('s':c:r:rs) = parseRule rs . replace c r
parseRule ('*':c:s:rs) = parseRule rs . twoWayReplace c s

-- | Memory functions
-- |  I didn't implement them, because they can't be implemented the
-- |  way that rules work right now and nobody seems to be using them anyways.
parseRule ('X':n:m:i:rs) = error ("Memory functions weren't implemented.")
parseRule ('4':rs) = error ("Memory functions weren't implemented.")
parseRule ('6':rs) = error ("Memory functions weren't implemented.")
parseRule ('M':rs) = error ("Memory functions weren't implemented.")

parseRule xs = error ("Failed to parse: " ++ show xs)

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

toDigit :: Char -> Int
toDigit c | c `elem` ['0'..'9'] = fromIntegral (toWord8 c - toWord8 '0')
          | c `elem` ['A'..'Z'] = fromIntegral (toWord8 c - toWord8 'A') + 10
          | otherwise           = error ("Invalid digit character: " ++ [c])

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
