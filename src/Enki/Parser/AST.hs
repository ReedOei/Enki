module Enki.Parser.AST where

import Enki.Types
import Enki.Parser.Util

newtype Expr = Expr { getId :: Id }
    deriving (Eq, Show, Read)

data Constraint = Constraint Id
                | Constraints [Constraint]
                | When Constraint Constraint
    deriving (Eq, Show, Read)

data Field = Field Id Type
    deriving (Eq, Show, Read)

data Constructor = Constructor Id [Field]
    deriving (Eq, Show, Read)

data Def = Func Id Constraint Expr
         | Rule Id Constraint
         | Data Id [Constructor]
         | Exec Constraint
         | Module String [Def]
         | NoImport String
         | Alias Id Id
    deriving (Eq, Show, Read)

