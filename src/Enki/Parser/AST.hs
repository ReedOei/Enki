module Enki.Parser.AST where

import Enki.Types
import Enki.Parser.Util

newtype Expr = Expr { getId :: Id }
    deriving (Eq, Show)

data Constraint = Constraint Id
                | Constraints [Constraint]
                | When Constraint Constraint
    deriving (Eq, Show)

data Field = Field Id Type
    deriving (Eq, Show)

data Constructor = Constructor Id [Field]
    deriving (Eq, Show)

data Def = Func Id Constraint Expr
         | Rule Id Constraint
         | Data Id [Constructor]
         | Exec Constraint
         | Module String [Def]
         | NoImport String
    deriving (Eq, Show)

