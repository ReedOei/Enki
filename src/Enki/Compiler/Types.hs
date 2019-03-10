module Enki.Compiler.Types where

import Enki.Types

data TypedId = StringVal String
             | IntVal Integer
             | BoolVal Bool
             | VarVal Type String
             | CompVal Type [TypedId]
    deriving (Eq, Show)

newtype TypedExpr = TypedExpr { exprId :: TypedId }
    deriving (Eq, Show)

data TypedConstraint = TypedConstraint TypedId
                     | TypedConstraints [TypedConstraint]
                     | TypedWhen TypedConstraint TypedConstraint
    deriving (Eq, Show)

data TypedField = TypedField Id Type
    deriving (Eq, Show)

data TypedConstructor = TypedConstructor TypedId [TypedField]
    deriving (Eq, Show)

data TypedDef = TypedFunc Id Type TypedConstraint TypedExpr
              | TypedRule Id Type TypedConstraint
              | TypedData Id Type [TypedConstructor]
              | TypedExec TypedConstraint
              | TypedModule String [TypedDef]
    deriving (Eq, Show)

