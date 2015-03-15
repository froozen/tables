{- |
Module: RuleImplementations

RuleImplementations implements the Rule functions specified at:
<https://hashcat.net/wiki/doku.php?id=rule_based_attack>

I tried to keep the names of the Rule functions consistent with the ones
specified on the Hashcat website. Each Rule function's name is followed by an 'R'.
-}

module RuleImplementations where

import Data.Word (Word8(..))
import Data.Char
import Data.List
import Data.Bits
import Control.Monad.State (modify, put, get, gets)
import Control.Monad.Reader (ask)
import Rule

nothingR :: Rule ()
nothingR = liftR id

lowercaseR :: Rule ()
lowercaseR = liftR $ fmap toLower

uppercaseR :: Rule ()
uppercaseR = liftR $ fmap toUpper

capitalizeR :: Rule ()
capitalizeR = liftR $ capitalize

-- | Capitalizes the first Char and lowers the rest
capitalize :: String -> String
capitalize = applyAt 0 toUpper . fmap toLower

-- | Applies a function to the element at a specific index of a list
applyAt :: (Integral a) => a -> (b -> b) -> [b] -> [b]
applyAt _ _ []     = []
applyAt 0 f (x:xs) = f x : xs
applyAt n f (x:xs) = x : applyAt (n - 1) f xs

invertCapitalizeR :: Rule ()
invertCapitalizeR = liftR $ fmap toggle . capitalize

-- | Toggles a Char's case
toggle :: Char -> Char
toggle w = if isUpper w then toLower w else toUpper w

toggleCaseR :: Rule ()
toggleCaseR = liftR $ fmap toggle

toggleR :: (Integral a) => a -> Rule ()
toggleR n = liftR $ applyAt n toggle

reverseR :: Rule ()
reverseR = liftR reverse

duplicateR :: Rule ()
duplicateR = liftR $ concat . replicate 2

duplicateNR :: (Integral a) => a -> Rule ()
duplicateNR n = liftR $ concat . genericReplicate n

reflectR :: Rule ()
reflectR = liftR reflect
    where reflect s = s ++ (reverse s)

rotateLeftR :: Rule ()
rotateLeftR = liftR rotateLeft
    where rotateLeft [] = []
          rotateLeft s  = safeTail s ++ [head s]

-- | A safe implementation of tail that, instead of failing, returns an empty
--   list if the specified list is empty
safeTail :: [a] -> [a]
safeTail [] = []
safeTail (x:xs) = xs

rotateRightR :: Rule ()
rotateRightR = liftR rotateRight
    where rotateRight [] = []
          rotateRight s  = last s : safeInit s

-- | A safe implementation of init that, instead of failing, returns an empty
--   list if the specified list is empty
safeInit :: [a] -> [a]
safeInit [] = []
safeInit (x:[]) = []
safeInit (x:xs) = x:safeInit xs

appendCharacterR :: Char -> Rule ()
appendCharacterR c = liftR $ (++ [c])

prependCharacterR :: Char -> Rule ()
prependCharacterR c = liftR $ (c:)

truncateLeftR :: Rule ()
truncateLeftR = liftR $ safeTail

truncateRightR :: Rule ()
truncateRightR = liftR $ safeInit

deleteNR :: (Integral a) => a -> Rule ()
deleteNR n = liftR $ remove n

-- | Removes the element at a specified index from a list
remove :: (Integral a) => a -> [b] -> [b]
remove n s
    | n `fits` genericLength s =
        let (xs, ys) = genericSplitAt n s
        in xs ++ (safeTail ys)
    | otherwise = s

deleteRangeR :: (Integral a) => a -> a -> Rule ()
deleteRangeR n m = liftR $ removeRange n m

-- | Removes the elements at a specified range from a list
removeRange :: (Integral a) => a -> a -> [b] -> [b]
removeRange n m s | n < m     = repeatRemove n (m - n) s
                  | otherwise = s
    where repeatRemove _ 0 s = s
          repeatRemove x n s = remove x $ repeatRemove x (n - 1) s

insertR :: (Integral a) => a -> Char -> Rule ()
insertR n c = liftR $ insertAt n c

-- | Inserts an element at a specified index of a list
insertAt :: (Integral a) => a -> b -> [b] -> [b]
insertAt 0 b xs     = b:xs
insertAt _ _ []     = []
insertAt n b (x:xs) = x : insertAt (n - 1) b xs

overwriteNR :: (Integral a) => a -> Char -> Rule ()
overwriteNR n c = liftR $ applyAt n $ const c

truncateNR :: (Integral a) => a -> Rule ()
truncateNR n = liftR $ genericTake n

replaceR :: Char -> Char -> Rule ()
replaceR x y = liftR $ replace x y
    where replace :: (Eq a) => a -> a -> [a] -> [a]
          replace x y s = map rp s
              where rp c | c == x    = y
                         | otherwise = c

purgeR :: Char -> Rule ()
purgeR c = liftR $ filter (== c)

duplicateFirstR :: (Integral a) => a -> Rule ()
duplicateFirstR n = liftR $ duplicateFirst n
    where duplicateFirst _ [] = []
          duplicateFirst n xs = genericReplicate n (head xs) ++ xs

duplicateLastR :: (Integral a) => a -> Rule ()
duplicateLastR n = liftR $ duplicateLast n
    where duplicateLast _ [] = []
          duplicateLast n xs = xs ++ (genericReplicate n (last xs))

duplicateAllR :: Rule ()
duplicateAllR = liftR $ concat . map (replicate 2)

extractMemoryR :: (Integral a) => a -> a -> a -> Rule ()
extractMemoryR n m i = modify $ \s -> s { result = extract (stored s) (result s) }
    where extract mem s = begin s ++ inserted mem ++ end s
          begin s = genericTake i s
          inserted mem = genericTake m . genericDrop n $ mem
          end s = genericDrop i s

appendMemoryR :: Rule ()
appendMemoryR = modify $ \s -> s { result = result s ++ stored s }

prependMemoryR :: Rule ()
prependMemoryR = modify $ \s -> s { result = stored s ++ result s }

memorizeR :: Rule ()
memorizeR = do
    s <- get
    original <- ask
    put s { stored = result s, result = original }

swapFrontR :: Rule ()
swapFrontR = liftR $ swap 0 1

-- | Swaps the elements at the specified indexes of a list
swap :: (Integral a) => a -> a -> [b] -> [b]
swap n m s
    | n `fits` genericLength s && m `fits` genericLength s =
        applyAt n (const (s `genericIndex` m)) .
        applyAt m (const (s `genericIndex` n)) $ s
    | otherwise = s

swapBackR :: Rule ()
swapBackR = liftR $ swapBack
    where swapBack s = swap (length s - 2) (length s - 1) s

swapNR :: (Integral a) => a -> a -> Rule ()
swapNR x y = liftR $ swap x y

bitShiftLeftR :: (Integral a) => a -> Rule ()
bitShiftLeftR n = liftR $ applyAt n $ modifyWord8 (`shift` 1)

-- | A helper function for applying a function to the ASCII
--   value of a Char
modifyWord8 :: (Word8 -> Word8) -> Char -> Char
modifyWord8 f = fromWord8 . f . toWord8
    where toWord8 = fromIntegral . ord
          fromWord8 = chr . fromIntegral

bitShiftRightR :: (Integral a) => a -> Rule ()
bitShiftRightR n = liftR $ applyAt n $ modifyWord8 (`shiftR` 1)

asciiIncrementR :: (Integral a) => a -> Rule ()
asciiIncrementR n = liftR $ applyAt n $ modifyWord8 (+1)

asciiDecrementR :: (Integral a) => a -> Rule ()
asciiDecrementR n = liftR $ applyAt n $ modifyWord8 (+ (-1))

replaceNPlusR :: (Integral a) => a -> Rule ()
replaceNPlusR n = liftR $ snatch n 1

-- | Replaces the element at a specified index with the element at a specified
--   relative index in a list
snatch :: (Integral a) => a -> a -> [b] -> [b]
snatch n r s
    | n `fits` genericLength s && (n + r) `fits` genericLength s =
        applyAt n (const (s `genericIndex` (n + r))) s
    | otherwise = s

-- | Determines wether an index "fits" into a length
fits :: (Num a, Ord a) => a -> a -> Bool
a `fits` b = a `inRange` (0, b - 1)

-- | Determines wether something is within a specified []-Range
inRange :: (Ord a) => a -> (a, a) -> Bool
a `inRange` (b, c) = a >= b && a <= c

replaceNMinusR :: (Integral a) => a -> Rule ()
replaceNMinusR n = liftR $ snatch n (-1)

duplicateBlockFrontR :: (Integral a) => a -> Rule ()
duplicateBlockFrontR n = liftR $ duplicateBlockFront n
    where duplicateBlockFront n s = genericTake n s ++ s

duplicateBlockBackR :: (Integral a) => a -> Rule ()
duplicateBlockBackR n = liftR $ duplicateBlockBack n
    where duplicateBlockBack n s = s ++ (reverse . genericTake n . reverse $ s)

titleR :: Rule ()
titleR = liftR $ title
    where title = unwords . fmap capitalize . words

rejectLessR :: (Integral a) => a -> Rule ()
rejectLessR n = guardR $ rejectLonger n
    where rejectLonger n s = (genericLength . result $ s) <= n

rejectGreaterR :: (Integral a) => a -> Rule ()
rejectGreaterR n = guardR $ rejectShorter n
    where rejectShorter n s = (genericLength . result $ s) >= n

rejectContainR :: Char -> Rule ()
rejectContainR c = guardR $ rejectContain c
    where rejectContain c = notElem c . result

rejectNotContainR :: Char -> Rule ()
rejectNotContainR c = guardR $ rejectContain c
    where rejectContain c = elem c . result

rejectEqualFirstR :: Char -> Rule ()
rejectEqualFirstR c = guardR $ rejectEqualFirst c
    where rejectEqualFirst c s = (head . result $ s) == c

rejectEqualLastR :: Char -> Rule ()
rejectEqualLastR c = guardR $ rejectEqualLast c
    where rejectEqualLast c s  = (last . result $ s) == c


rejectEqualAtR :: (Integral a) => a -> Char -> Rule ()
rejectEqualAtR n c = guardR $ rejectEqualAt n c
    where rejectEqualAt n c s = (genericLength . result $ s) >= n
                && (result s) `genericIndex` n == c

rejectContainsR :: (Integral a) => a -> Char -> Rule ()
rejectContainsR n c = guardR $ rejectContains n c
    where rejectContains n c s = (genericLength . filter (==c) . result $ s) < n

rejectEqualsMemoryR :: Rule ()
rejectEqualsMemoryR = guardR $ rejectEqualsMemory
    where rejectEqualsMemory s = result s == stored s
