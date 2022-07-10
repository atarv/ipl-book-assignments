#!/usr/bin/env stack
{- stack script
 --resolver lts-18.8
 --ghc-options -Wall
 --package "array containers"
 --compile
 --copy-bins
-}
module Main where

import           System.Environment
import           TypeChecker

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> checkFile filePath
        _          -> putStrLn "Usage: lab2 <path-to-cpp-source-file>"

checkFile :: FilePath -> IO ()
checkFile file = do
    parseResult <- parse <$> readFile file
    case parseResult of
        Left  err  -> putStrLn ("SYNTAX ERROR\n" <> err)
        Right prog -> case typecheck prog of
            Left  err -> putStrLn ("TYPE ERROR\n" <> err)
            Right _   -> putStrLn "OK"
