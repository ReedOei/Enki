module Enki.Parser.AST where

import Enki.Types
import Enki.Parser.Util

data Expr = Expr Id
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

class PrettyPrint a where
    prettyPrint :: a -> String

maudeList :: [String] -> String
maudeList [] = "nil"
maudeList xs@(_:_) = unwords xs

instance PrettyPrint Id where
    prettyPrint (S str)      = "s(" ++ show str ++ ")"
    prettyPrint (I i)        = "i(" ++ show i ++ ")"
    prettyPrint (B b)        = "b(" ++ toLowerCase (show b) ++ ")"
    prettyPrint (V name)     = "v(" ++ show name ++ ")"
    prettyPrint (Comp parts) = "comp(" ++ maudeList (map prettyPrint parts) ++ ")"

instance PrettyPrint Expr where
    prettyPrint (Expr id) = "e(" ++ prettyPrint id ++ ")"

instance PrettyPrint Constraint where
    prettyPrint (Constraint id)       = "c(" ++ prettyPrint id ++ ")"
    prettyPrint (Constraints cs)      = "cs(" ++ maudeList (map prettyPrint cs) ++ ")"
    prettyPrint (When condition body) = "when(" ++ prettyPrint condition ++ "," ++ prettyPrint body ++ ")"

instance PrettyPrint Type where
    prettyPrint EnkiInt          = "int"
    prettyPrint EnkiBool         = "bool"
    prettyPrint EnkiString       = "string"
    prettyPrint (Any s)          = "any(\"" ++ s ++ "\")"
    prettyPrint (FuncType t1 t2) = "func(" ++ prettyPrint t1 ++ "," ++ prettyPrint t2 ++ ")"
    prettyPrint (RuleType t1 t2) = "rule(" ++ prettyPrint t1 ++ "," ++ prettyPrint t2 ++ ")"
    prettyPrint (DataType t1 t2) = "data(" ++ prettyPrint t1 ++ "," ++ prettyPrint t2 ++ ")"
    prettyPrint (TypeName id)    = "type(" ++ prettyPrint id ++ ")"

instance PrettyPrint Field where
    prettyPrint (Field id fieldType) = "field(" ++ prettyPrint id ++ "," ++ prettyPrint fieldType ++ ")"

instance PrettyPrint Constructor where
    prettyPrint (Constructor id fields) = "constructor(" ++ prettyPrint id ++ "," ++ maudeList (map prettyPrint fields) ++ ")"

instance PrettyPrint Def where
    prettyPrint (Func id c e) = "def(f(" ++ prettyPrint id ++ "," ++ prettyPrint c ++ "," ++ prettyPrint e ++ "))"
    prettyPrint (Rule id c)   = "def(r(" ++ prettyPrint id ++ "," ++ prettyPrint c ++ "))"
    prettyPrint (Data id constrs) = "def(d(" ++ prettyPrint id ++ "," ++ maudeList (map prettyPrint constrs) ++ "))"
    prettyPrint (Exec c)   = "exec(ex(" ++ prettyPrint c ++ "))"
    prettyPrint (Module _ defs) = "import(m(" ++ maudeList (map prettyPrint defs) ++ "))"

