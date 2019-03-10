module Enki.Compiler.Types where

import Data.Map (Map)
import qualified Data.Map as Map

import Enki.Types
import Enki.Parser.AST

data TypedId = StringVal String
             | IntVal Integer
             | BoolVal Bool
             | VarVal String
             | FuncCall TypedDef (Map String TypedId)
             | BinOp String Type TypedId TypedId
    deriving (Eq, Show)

newtype TypedExpr = TypedExpr { exprId :: TypedId }
    deriving (Eq, Show)

data TypedConstraint = TypedConstraint TypedId
                     | TypedConstraints [TypedConstraint]
                     | TypedWhen TypedConstraint TypedConstraint
    deriving (Eq, Show)

data TypedDef = TypedFunc Id Type TypedConstraint TypedExpr
              | TypedRule Id Type TypedConstraint
              | TypedData Id Type [Constructor]
              | TypedExec TypedConstraint
              | TypedModule String [TypedDef]
    deriving (Eq, Show)

