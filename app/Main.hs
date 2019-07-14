module Main where

import Control.Monad.Trans.State.Lazy

import System.Environment
import System.Process

import Enki.Compiler

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["run", fname] -> do
            source <- compile fname
            let outputName = fname ++ "_out.pl"
            writeFile outputName source
            callProcess "swipl" [outputName]

        [fname] -> putStrLn =<< compile fname

        _ -> putStrLn $ "Usage:\n" ++
            "enki FILE\n" ++
            "enki run FILE"

