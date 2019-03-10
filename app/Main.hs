module Main where

import System.Environment

import Enki.Parser.Parser

main :: IO ()
main = do
    args <- getArgs

    case args of
        [fname] -> parseFile fname Nothing
        [fname, output] -> parseFile fname $ Just output
        _ -> putStrLn $ "Usage: Parser FILE [OUTPUT_FILE]"

