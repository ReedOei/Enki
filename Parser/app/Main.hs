module Main where

import System.Environment

import Parser

main :: IO ()
main = do
    args <- getArgs

    case args of
        [fname, output] -> parseFile fname output
        _ -> putStrLn $ "Usage: Parser FILE OUTPUT_FILE"

