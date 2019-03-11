module Main where

import Control.Monad.Trans.State.Lazy

import System.Environment

import Enki.Compiler

main :: IO ()
main = do
    args <- getArgs

    case args of
        [fname] -> putStrLn =<< compile fname
        _ -> putStrLn $ "Usage: Parser FILE [OUTPUT_FILE]"

