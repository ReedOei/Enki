module Main where

import Control.Monad.Trans.State.Lazy

import System.Environment
import System.Process

import Enki.Compiler

genExec :: FilePath -> FilePath -> IO FilePath
genExec inFile outFile = generateExecutable inFile outFile

main :: IO ()
main = do
    args <- getArgs

    case args of
        ["run", fname] -> do
            execName <- genExec fname (fname ++ ".out")
            callProcess execName []

        ["run", fname, outname] -> do
            execName <- genExec fname outname
            callProcess execName []

        ["compile", fname, "to", outname] -> do
            genExec fname outname
            pure ()

        [fname] -> putStrLn =<< compile fname

        _ -> putStrLn $ "Usage:\n" ++
            "   enki FILE\n" ++
            "   enki run FILE [OUTPUT FILE NAME]\n" ++
            "   enki compile FILE to OUTPUT_FILE_NAME"

