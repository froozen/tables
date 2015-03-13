{-# LANGUAGE DoAndIfThenElse #-}

{- |
 Module: Table

Table implements all the functions used for creating and doings lookups
in a table.

A table is a directory structure, that has a directory for every character
of the key.
-}

module Table
    ( createTableEntry
    , lookUpHash
    ) where

import System.FilePath ((</>))
import System.Directory (doesDirectoryExist, createDirectory, doesFileExist)
import Data.List (foldl')
import Control.Exception (handle, IOException(..))

import Hashing (HashPair(..))

-- | Generates the filepath corresponding to a String key
--
-- >>> generatePath "dir" "test"
-- "dir/t/e/s/t"
generatePath :: FilePath -> String -> FilePath
generatePath path = foldl' foldingFunc path
    where foldingFunc acc c = acc </> [c]

-- | Inserts a HashPair into a table
createTableEntry :: FilePath -> HashPair -> IO ()
createTableEntry dir pair@(HashPair (plain, hash)) = do
    putStrLn . show $ pair
    let dirs = init . fmap (:[]) $ hash
    success <- createDirectoryPath dir dirs
    if success
    then do
        let path = generatePath dir hash
        writeFile path plain 
    else putStrLn ("Failure: " ++ hash)

-- | Recursivly creates a path of subfolders and returns False if it fails
createDirectoryPath :: FilePath -> [String] -> IO Bool
createDirectoryPath _ [] = return True
createDirectoryPath path (x:xs) = do
    let dir = path </> x
    exists <- doesDirectoryExist dir
    next <- if exists
        then return True
        else do
            handle (\e -> do
                    let typed = show (e :: IOException)
                    return False) $
                do
                    createDirectory dir
                    return True
    if next
    then createDirectoryPath dir xs
    else return False

-- | Looks up the value corresponding to a key in a table, returning it wrapped
--   in a Maybe, as it may not exist
lookUpHash :: FilePath -> String -> IO (Maybe String)
lookUpHash dir hash = do
    let path = generatePath dir hash
    exists <- doesFileExist path
    if exists
    then do
        plain <- readFile path
        return (Just plain)
    else return Nothing
