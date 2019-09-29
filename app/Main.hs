module Main where

import Control.Monad.Trans.State.Lazy

import System.Environment
import System.IO
import System.Process

import Enki.Compiler
import Enki.Parser.AST
import Enki.Parser.Parser

genExec :: FilePath -> FilePath -> IO FilePath
genExec inFile outFile = generateExecutable inFile outFile

runRepl :: IO ()
runRepl = do
    writeFile ".enki-repl.enki" ""
    runReplWith ".enki-repl.enki"

runFile fname = do
    execName <- genExec fname $ fname ++ ".out"
    callProcess execName []

runReplWith :: String -> IO ()
runReplWith fname = do
    putStr "> "
    input <- getLine
    parsed <- parseDef input

    -- If they input a definition, we should
    case parsed of
        Left err -> do
            putStrLn "Could not parse input!"
            print err
            runReplWith fname

        Right [Exec constr] -> do
            ls <- lines <$> readFile fname
            writeFile fname $ unlines $ ls ++ [input]
            runFile fname
            writeFile fname $ unlines ls -- Remove the new line
            runReplWith fname

        _ -> do
            ls <- lines <$> readFile fname
            writeFile fname $ unlines $ ls ++ [input]
            runFile fname
            runReplWith fname

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering

    -- TODO: Use a proper argument parsing library
    args <- getArgs

    case args of
        ["run", fname] -> runFile fname
        ["run", fname, outname] -> do
            execName <- genExec fname outname
            callProcess execName []

        ["compile", fname, "to", outname] -> do
            genExec fname outname
            pure ()

        ["help"] -> putStrLn $ "Usage:\n" ++
            "   enki FILE\n" ++
            "   enki run FILE [OUTPUT FILE NAME]\n" ++
            "   enki compile FILE to OUTPUT_FILE_NAME"

        [fname] -> putStrLn =<< compile fname

        [] -> runRepl

