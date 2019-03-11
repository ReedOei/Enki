module Enki.Compiler where

import Control.Monad.Trans.State.Lazy

import Enki.Parser.Parser
import Enki.Compiler.TypeChecker
import Enki.Compiler.CodeGen

compile :: String -> IO String
compile fname = do
    defs <- parseFileAst fname
    let (inferred,_) = runState (mapM infer defs) newEnv
    let (generated, _) = runState (mapM codeGen inferred) newCodeGenEnv

    pure $ prettyPrint $ PrologFile generated

