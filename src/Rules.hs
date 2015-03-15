{- |
Module: Rules

This module parses Hashcat Rules, as specified on the Hashcat website:
<https://hashcat.net/wiki/doku.php?id=rule_based_attack>

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
import Data.Char (ord)

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

-- | Helper function for parsing rules
contParse :: String -> Rule () -> RuleParser ()
contParse s r = tell r >> doParse s

-- | The actual parsing function
doParse :: String -> RuleParser ()
doParse (' ':rs) = doParse rs

-- Argumentless functions
doParse (':':rs) = contParse rs nothingR
doParse ('l':rs) = contParse rs lowercaseR
doParse ('u':rs) = contParse rs uppercaseR
doParse ('t':rs) = contParse rs toggleCaseR
doParse ('c':rs) = contParse rs capitalizeR
doParse ('r':rs) = contParse rs reverseR
doParse ('d':rs) = contParse rs duplicateR
doParse ('f':rs) = contParse rs reflectR
doParse ('{':rs) = contParse rs rotateLeftR
doParse ('}':rs) = contParse rs rotateRightR
doParse ('[':rs) = contParse rs truncateLeftR
doParse (']':rs) = contParse rs truncateRightR
doParse ('q':rs) = contParse rs duplicateAllR
doParse ('C':rs) = contParse rs capitalizeR
doParse ('k':rs) = contParse rs swapFrontR
doParse ('K':rs) = contParse rs swapBackR
doParse ('E':rs) = contParse rs titleR

-- One argument functions
doParse ('T':n:rs)  = contParse rs $ toggleR (toDigit n)
doParse ('p':n:rs)  = contParse rs $ duplicateNR (toDigit n)
doParse ('D':n:rs)  = contParse rs $ deleteNR (toDigit n)
doParse ('z':n:rs)  = contParse rs $ duplicateFirstR (toDigit n)
doParse ('Z':n:rs)  = contParse rs $ duplicateLastR (toDigit n)
doParse ('\'':n:rs) = contParse rs $ truncateNR (toDigit n)
doParse ('@':c:rs)  = contParse rs $ purgeR c
doParse ('$':c:rs)  = contParse rs $ appendCharacterR c
doParse ('^':c:rs)  = contParse rs $ prependCharacterR c
doParse ('L':n:rs)  = contParse rs $ bitShiftLeftR (toDigit n)
doParse ('R':n:rs)  = contParse rs $ bitShiftRightR (toDigit n)
doParse ('+':n:rs)  = contParse rs $ asciiIncrementR (toDigit n)
doParse ('-':n:rs)  = contParse rs $ asciiDecrementR (toDigit n)
doParse ('.':n:rs)  = contParse rs $ replaceNPlusR (toDigit n)
doParse (',':n:rs)  = contParse rs $ replaceNMinusR (toDigit n)
doParse ('y':n:rs)  = contParse rs $ duplicateBlockFrontR (toDigit n)
doParse ('Y':n:rs)  = contParse rs $ duplicateBlockBackR (toDigit n)

-- Two argument functions
doParse ('x':n:m:rs) = contParse rs $ deleteRangeR (toDigit n) (toDigit m)
doParse ('i':n:c:rs) = contParse rs $ insertR (toDigit n) c
doParse ('o':n:c:rs) = contParse rs $ overwriteNR (toDigit n) c
doParse ('s':c:r:rs) = contParse rs $ replaceR c r
doParse ('*':x:y:rs) = contParse rs $ swapNR (toDigit x) (toDigit y)

-- Memory functions
doParse ('X':n:m:i:rs) = contParse rs $ extractMemoryR (toDigit n) (toDigit m) (toDigit i)
doParse ('4':rs)       = contParse rs $ appendMemoryR
doParse ('6':rs)       = contParse rs $ prependMemoryR
doParse ('M':rs)       = contParse rs $ memorizeR

-- Rejection functions
doParse ('<':n:rs)   = contParse rs $ rejectLessR (toDigit n)
doParse ('>':n:rs)   = contParse rs $ rejectGreaterR (toDigit n)
doParse ('!':c:rs)   = contParse rs $ rejectContainR c
doParse ('/':c:rs)   = contParse rs $ rejectNotContainR c
doParse ('(':c:rs)   = contParse rs $ rejectEqualFirstR c
doParse (')':c:rs)   = contParse rs $ rejectEqualLastR c
doParse ('=':n:c:rs) = contParse rs $ rejectEqualAtR (toDigit n) c
doParse ('%':n:c:rs) = contParse rs $ rejectContainsR (toDigit n) c
doParse ('Q':rs) = contParse rs $ rejectEqualsMemoryR

doParse [] = return ()
doParse xs = throwE $ "invalid input at: " ++ xs

-- | Turns a Char into an Int using the Hashcat digit format
toDigit :: Char -> Int
toDigit c
    | c `elem` ['0'..'9'] = fromIntegral (toWord8 c - toWord8 '0')
    | c `elem` ['A'..'Z'] = fromIntegral (toWord8 c - toWord8 'A') + 10
    | otherwise           = error ("Invalid digit character: " ++ [c])
    where toWord8 = fromIntegral . ord
