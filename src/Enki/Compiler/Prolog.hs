module Enki.Compiler.Prolog where

data PrologFile = PrologFile String [PrologDef]
    deriving (Eq, Show)

data PrologDef = Predicate String String [String] [PrologConstraint]
               | Main [PrologConstraint]
    deriving (Eq, Show)

data PrologExpr = PrologInt Integer
                | PrologAtom String
                | PrologVar String
                | PrologOpExpr String PrologExpr PrologExpr
                | PrologFunctor String [PrologExpr]
                | PrologLambda [String] [String] PrologConstraint
    deriving (Eq, Show)

data PrologConstraint = PredCall String [PrologExpr]
                      | PrologOp String PrologExpr PrologExpr
                      | Condition [PrologConstraint] [PrologConstraint]
                      | Disjunction [PrologConstraint] [PrologConstraint]
                      | Conjunction [PrologConstraint] [PrologConstraint]
    deriving (Eq, Show)

