module Main where

import Control.Monad.Trans.State.Lazy

import System.Environment
import System.IO
import System.Process

import Enki.Compiler
import Enki.Parser.AST
import Enki.Parser.Parser

runRepl :: IO ()
runRepl = do
    writeFile ".enki-repl.enki" ""
    runReplWith ".enki-repl.enki"

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

    args <- getArgs

    case args of
        ["run", fname] -> runFile fname
        ["run", fname, outname] -> do
            execName <- generateExecutable fname outname
            callProcess execName []

        ["parse", fname] -> print =<< parseFileAst fname

        ["compile", fname, outname] -> do
            generateExecutable fname outname
            pure ()

        ["help"] -> putStrLn $ "Usage:\n" ++
            "   enki FILE\n" ++
            "   enki run FILE [OUTPUT FILE NAME]\n" ++
            "   enki compile FILE OUTPUT_FILE_NAME"

        [fname] -> do
            res <- compile fname
            case res of
                Left errs -> mapM_ print errs
                Right sourceCode -> putStrLn sourceCode

        [] -> runRepl

