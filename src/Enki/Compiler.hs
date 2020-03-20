{-# LANGUAGE LambdaCase #-}

module Enki.Compiler where

import Control.Monad.Trans.State.Lazy

import Data.List

import Enki.Parser.Parser
import Enki.Compiler.CodeGen
import Enki.Compiler.Prolog
import Enki.Compiler.Types
import Enki.Compiler.TypeChecker

import System.Directory
import System.Environment
import System.FilePath.Posix
import System.Process
import System.TimeIt

runInfer :: String -> IO (Either [Error] [TypedDef])
runInfer src =
    parseDef src >>= \case
        Left err -> pure $ Left [ErrorMsg $ show err]
        Right defs -> runError (mapM infer defs) newEnv

compile :: FilePath -> IO (Either [Error] String)
compile fname = do
    defs <- parseFileAst fname

    typeRes <- runError (mapM infer defs) newEnv

    case typeRes of
        Left errs -> pure $ Left errs
        Right inferred -> do
            genRes <- runError (mapM codeGen inferred) newCodeGenEnv

            case genRes of
                Left errs -> pure $ Left errs
                Right generated -> do
                    enkiPath <- getEnv "ENKI_PATH"
                    -- Load the standard library (the prolog part)
                    -- TODO: Replace this constant "base.pl"
                    prologLibrary <- withCurrentDirectory enkiPath $ readFile "base.pl"

                    pure $ Right $ intercalate "\n" $ prettyPrint $ PrologFile prologLibrary $ concat generated

compileTime = timeItNamed "Enki compile time"
prologCompileTime = timeItNamed "SWI-Prolog compile time"

generateProlog :: FilePath -> IO FilePath
generateProlog fname = do
    source <- compileTime $ compile fname

    let outputName = fname ++ "_out.pl"

    case source of
        Left errs -> mapM_ print errs
        Right sourceCode -> writeFile outputName sourceCode

    pure outputName

generateExecutable :: FilePath -> FilePath -> IO FilePath
generateExecutable fname outputFile = do
    outputName <- generateProlog fname
    prologCompileTime $ callProcess "swipl" ["-o", outputFile, "-c", outputName]

    pure $
        if "/" `isPrefixOf` outputFile then
            outputFile
        else
            "./" ++ outputFile

runFile :: FilePath -> IO ()
runFile fname = do
    outputName <- generateProlog fname
    callProcess "swipl" [outputName]

