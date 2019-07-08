{-# LANGUAGE LambdaCase #-}

module Enki.Compiler where

import Control.Monad.Trans.State.Lazy

import Data.List

import Enki.Parser.Parser
import Enki.Compiler.Types
import Enki.Compiler.TypeChecker
import Enki.Compiler.CodeGen

import System.Directory
import System.Environment
import System.FilePath.Posix

runInfer :: String -> IO [TypedDef]
runInfer src =
    parseDef src >>= \case
        Left err -> error $ show err
        Right defs -> pure $ evalState (mapM infer defs) newEnv

compile :: String -> IO String
compile fname = do
    defs <- parseFileAst fname
    let (inferred,_) = runState (mapM infer defs) newEnv
    let (generated, _) = runState (mapM codeGen inferred) newCodeGenEnv

    enkiPath <- getEnv "ENKI_PATH"
    -- Load the standard library (the prolog part)
    -- TODO: Replace this constant "base.pl"
    prologLibrary <- withCurrentDirectory enkiPath $ readFile "base.pl"

    pure $ intercalate "\n" $ prettyPrint $ PrologFile prologLibrary $ concat generated

