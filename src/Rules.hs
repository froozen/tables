{- |
Module: Rules

This module parses Hashcat Rules, as specified on the Hashcat website:
https://hashcat.net/wiki/doku.php?id=rule_based_attack

Rules acts as the interface for the Rule module by reexporting Rule and
applyRule and implements the Rule Parsing via parseRule.

The parsing is done in the Writer monad, which is used to chain the rule
functions together, wrapped in an ExceptT String monad transformer, to
account for the possibility of malformed rules.
-}

module Rules
    ( Rule
    , applyRule
    , parseRule
    ) where

import Control.Monad.Trans.Except
import Control.Monad.Writer
import Data.Char
import Data.Bits

import Rule
import RuleImplementations

-- | Monad used to do the Rule Parsing
type RuleParser = ExceptT String (Writer (Rule ()))

-- | Parse a String into a Rule
--   Uses Either to account for malformed rules and other errors
parseRule :: String -> Either String (Rule ())
parseRule s = case fst parsed of
        Left err -> Left err
        Right _  -> Right $ snd parsed
    where parsed = runWriter (runExceptT $ doParse s)

-- | Helper function for parsing simple (String -> String) rules
contParse :: String -> (String -> String) -> RuleParser ()
contParse s f = tell (liftR f) >> doParse s

-- | The actual parsing function
doParse :: String -> RuleParser ()
doParse (' ':rs) = doParse rs

-- Argumentless functions
doParse (':':rs) = contParse rs $ id
doParse ('l':rs) = contParse rs $ fmap toLower
doParse ('u':rs) = contParse rs $ fmap toUpper
doParse ('t':rs) = contParse rs $ fmap toggle
doParse ('c':rs) = contParse rs $ capitalize
doParse ('r':rs) = contParse rs $ reverse
doParse ('d':rs) = contParse rs $ (\s -> s ++ s)
doParse ('f':rs) = contParse rs $ (\s -> s ++ reverse s)
doParse ('{':rs) = contParse rs $ rotateLeft
doParse ('}':rs) = contParse rs $ rotateRight
doParse ('[':rs) = contParse rs $ safeTail
doParse (']':rs) = contParse rs $ safeInit
doParse ('q':rs) = contParse rs $ (\s -> concat . map (replicate 2) $ s)
doParse ('C':rs) = doParse ('c':'t':rs)
doParse ('k':rs) = contParse rs $ swap 0 1
doParse ('K':rs) = contParse rs $ (\s -> swap (length s - 2) (length s - 1) s)
doParse ('E':rs) = contParse rs $ title

-- One argument functions
doParse ('T':n:rs)  = contParse rs $ applyAt (toDigit n) toggle
doParse ('p':n:rs)  = contParse rs $ concat . replicate (toDigit n)
doParse ('D':n:rs)  = contParse rs $ remove (toDigit n)
doParse ('z':n:rs)  = contParse rs $ duplicateFirst (toDigit n)
doParse ('Z':n:rs)  = contParse rs $ duplicateLast (toDigit n)
doParse ('\'':n:rs) = contParse rs $ take (toDigit n)
doParse ('@':c:rs)  = contParse rs $ (\s -> filter (== c) s)
doParse ('$':c:rs)  = contParse rs $ (\s -> s ++ [c])
doParse ('^':c:rs)  = contParse rs $ (c :)
doParse ('R':n:rs)  = contParse rs $ applyAt (toDigit n) $ modifyWord8 (`shift` (-1))
doParse ('L':n:rs)  = contParse rs $ applyAt (toDigit n) $ modifyWord8 (`shift` 1)
doParse ('+':n:rs)  = contParse rs $ applyAt (toDigit n) $ modifyWord8 (+1)
doParse ('-':n:rs)  = contParse rs $ applyAt (toDigit n) $ modifyWord8 (+ (-1))
doParse ('.':n:rs)  = contParse rs $ snatch (toDigit n) 1
doParse (',':n:rs)  = contParse rs $ snatch (toDigit n) (-1)
doParse ('y':n:rs)  = contParse rs $ (\s -> (take (toDigit n) s) ++ s)
doParse ('Y':n:rs)  = contParse rs $ (\s -> s ++ (reverse . take (toDigit n) . reverse $ s))

-- Two argument functions
doParse ('x':n:m:rs) = contParse rs $ removeRange (toDigit n) (toDigit m)
doParse ('i':n:c:rs) = contParse rs $ insertAt (toDigit n) (c)
doParse ('o':n:c:rs) = contParse rs $ applyAt (toDigit n) (const c)
doParse ('s':c:r:rs) = contParse rs $ replace c r
doParse ('*':c:s:rs) = contParse rs $ twoWayReplace c s

-- Memory functions
doParse ('X':n:m:i:rs) = tell (extractMem (toDigit n) (toDigit m) (toDigit i)) >> doParse rs
doParse ('4':rs) = tell appendMem >> doParse rs
doParse ('6':rs) = tell prependMem >> doParse rs
doParse ('M':rs) = tell saveMem >> doParse rs

-- Rejection functions
doParse ('<':n:rs) = tell (guardR $ \s -> (length . result $ s) <= toDigit n) >> doParse rs
doParse ('>':n:rs) = tell (guardR $ \s -> (length . result $ s) >= toDigit n) >> doParse rs
doParse ('!':c:rs) = tell (guardR (notElem c . result)) >> doParse rs
doParse ('/':c:rs) = tell (guardR (elem c . result)) >> doParse rs
doParse ('(':c:rs) = tell (guardR $ \s -> (head . result $ s) == c) >> doParse rs
doParse (')':c:rs) = tell (guardR $ \s -> (last . result $ s) == c) >> doParse rs
doParse ('=':n:c:rs) = do
    tell (guardR $ \s -> (length . result $ s) >= toDigit n
                          && (result s) !! (toDigit n) == c)
    doParse rs
doParse ('%':n:c:rs) = do
    tell (guardR $ \s -> (length . filter (==c) . result $ s) < (toDigit n))
    doParse rs

doParse [] = return ()
doParse xs = throwE $ "invalid input at: " ++ xs

-- | Turns a Char into an Int using the Hashcat digit format
toDigit :: Char -> Int
toDigit c | c `elem` ['0'..'9'] = fromIntegral (toWord8 c - toWord8 '0')
          | c `elem` ['A'..'Z'] = fromIntegral (toWord8 c - toWord8 'A') + 10
          | otherwise           = error ("Invalid digit character: " ++ [c])
