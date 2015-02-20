{-# LANGUAGE DoAndIfThenElse #-}

module Table
    ( createTableEntry
    , lookUpHash
    ) where

import System.FilePath ((</>))
import System.Directory (doesDirectoryExist, createDirectory, doesFileExist)
import Data.List (foldl')
import Control.Exception (handle, IOException(..))

import Hashing (HashPair(..))

generatePath :: FilePath -> String -> FilePath
generatePath path = foldl' foldingFunc path
    where foldingFunc acc c = acc </> [c]

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

lookUpHash :: FilePath -> String -> IO (Maybe String)
lookUpHash dir hash = do
    let path = generatePath dir hash
    exists <- doesFileExist path
    if exists
    then do
        plain <- readFile path
        return (Just plain)
    else return Nothing
