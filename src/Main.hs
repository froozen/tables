{-# LANGUAGE DoAndIfThenElse #-}

module Main where

import System.Directory (doesFileExist, doesDirectoryExist)
import System.Environment (getArgs)
import Control.Applicative ((<*>))
import Control.Monad (mapM, forM, forM_)
import Data.List (isPrefixOf)
import Control.Arrow (left)
import Data.Either (rights)
import Data.Maybe (catMaybes)

import Rules
import Hashing
import Table

main = do
    args <- getArgs
    case args of
        ["create", rulePath, wordlistPath, tableDir] -> create rulePath wordlistPath tableDir
        ("create":_) -> putStrLn "Usage: tables create <rulePath> <wordlistPath> <tableDir>"

        ["lookup", hashesPath, tableDir] -> lookUp hashesPath tableDir
        ("lookup":_) -> putStrLn "Usage: tables lookup <hashesPath> <tableDir>"

        _ -> putStrLn "Usage: tables [create/lookup]"

create :: FilePath -> FilePath -> FilePath -> IO ()
create rulePath wordlistPath tableDir = do
    tableDirExists <- doesDirectoryExist tableDir
    if tableDirExists
    then do
        rules <- loadRules rulePath
        words <-  loadWordlist wordlistPath
        let processed = catMaybes $ [applyRule rule word | rule <- rules, word <- words]
        let hashed = map pair processed
        mapM (createTableEntry tableDir) hashed
        return ()
    else putStrLn ("Invalid table-dir: " ++ tableDir)

loadRules :: FilePath -> IO [Rule]
loadRules path = do
    exists <- doesFileExist path
    if exists
    then do
        contents <- readFile path
        let ruleLines = filter validLine . lines . unixify $ contents
        let rules = map parseRule ruleLines
        forM_ rules $ \rule -> do
            case rule of
                Left err -> putStrLn $ "Error :" ++ err
                Right _ -> return ()
        return . rights $ rules
    else do
        putStrLn ("Invalid rule path: " ++ path)
        return []

loadWordlist :: FilePath -> IO [String]
loadWordlist path = do
    exists <- doesFileExist path
    if exists
    then do
        contents <- readFile path
        let words = lines . unixify $ contents
        return words
    else do
        putStrLn ("Invalid wordlist path: " ++ path)
        return []

validLine :: String -> Bool
validLine s = not ("#" `isPrefixOf` s || null s)

unixify :: String -> String
unixify = filter (/= '\r')

lookUp :: FilePath -> FilePath -> IO ()
lookUp hashesPath tableDir = do
    hashesExist <- doesFileExist hashesPath
    if hashesExist
    then do
        tableExists <- doesDirectoryExist tableDir
        if tableExists
        then do
            contents <- readFile hashesPath
            let hashes = lines . unixify $ contents
            forM hashes $ (\hash -> do
                result <- lookUpHash tableDir hash
                case result of
                    Just plain -> putStrLn (hash ++ " -> " ++ plain)
                    Nothing -> return ()
                )
            return ()
        else putStrLn ("Invalid table dir: " ++ tableDir)
    else putStrLn ("Invalid hashes path: " ++ hashesPath)
