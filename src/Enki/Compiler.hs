{-# LANGUAGE LambdaCase #-}

module Enki.Compiler where

import Control.Monad.Trans.State.Lazy

import Enki.Parser.Parser
import Enki.Compiler.Types
import Enki.Compiler.TypeChecker
import Enki.Compiler.CodeGen

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

    pure $ prettyPrint $ PrologFile generated

