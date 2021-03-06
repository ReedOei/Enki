module Enki.Compiler.Types where

import Control.Monad.Trans.State.Lazy

import Data.Map (Map)
import qualified Data.Map as Map

import Enki.Types
import Enki.Parser.AST

data TypedId = StringVal String
             | IntVal Integer
             | VarVal String
             | FuncCall TypedDef (Map String TypedId)
             | FuncRef TypedDef Type (Map String TypedId) [String]
             | BinOp String Type TypedId TypedId
    deriving (Eq, Show, Read)

newtype TypedExpr = TypedExpr { exprId :: TypedId }
    deriving (Eq, Show, Read)

data TypedConstraint = TypedConstraint TypedId
                     | TypedConstraints [TypedConstraint]
                     | TypedWhen TypedConstraint TypedConstraint
    deriving (Eq, Show, Read)

data TypedDef = TypedFunc Id Type TypedConstraint TypedExpr
              | TypedRule Id Type TypedConstraint
              | TypedConstructor Id Type
              | TypedData Id [TypedDef]
              | TypedExec TypedConstraint
              | TypedModule String [TypedDef]
    deriving (Eq, Show, Read)

data Error = ErrorMsg String
    deriving (Eq, Show, Read)

class ErrorReporter env where
    reportError :: Monad m => a -> Error -> StateT env m a

    errorList :: env -> [Error]

    runError :: Monad m => StateT env m a -> env -> m (Either [Error] a)
    runError comp env = do
        (res, newEnv) <- runStateT comp env

        if not $ null $ errorList newEnv then
            pure $ Left $ errorList newEnv
        else
            pure $ Right res

typedIdVars :: TypedId -> [String]
typedIdVars (StringVal _) = []
typedIdVars (IntVal _) = []
typedIdVars (VarVal x) = [x]
typedIdVars (FuncCall _ varMap) = concatMap typedIdVars $ Map.elems varMap
typedIdVars (FuncRef _ _ freeVarMap placeHolders) = placeHolders ++ concatMap typedIdVars (Map.elems freeVarMap)
typedIdVars (BinOp _ _ id1 id2) = typedIdVars id1 ++ typedIdVars id2

vars :: Id -> [String]
vars (S _) = []
vars (I _) = []
vars (V str) = [str]
vars (Comp []) = []
vars (Comp (id:ids)) = vars id ++ vars (Comp ids)

defId :: TypedDef -> Id
defId (TypedFunc funcId _ _ _)      = funcId
defId (TypedRule ruleId _ _)        = ruleId
defId (TypedConstructor constrId _) = constrId
defId _                             = Comp []

paramsOf :: TypedDef -> [String]
paramsOf (TypedFunc funcId _ _ _) = vars funcId
paramsOf (TypedRule ruleId _ _) = vars ruleId
paramsOf (TypedData dataId _) = vars dataId
paramsOf _ = []

isFuncLike :: TypedDef -> Bool
isFuncLike TypedFunc{} = True
isFuncLike TypedConstructor{} = True
isFuncLike _ = False

