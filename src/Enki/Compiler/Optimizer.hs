module Enki.Compiler.Optimizer where

import Enki.Compiler.Prolog

class Optimizable a where
    optimize :: a -> [a]

instance Optimizable PrologFile where
    optimize (PrologFile include defs) = [PrologFile include $ concatMap optimize defs]

instance Optimizable PrologDef where
    optimize (Predicate name typeString args constraints) =
        [Predicate name typeString args $ concatMap optimize constraints]
    optimize (Main constraints) =
        case concatMap optimize constraints of
            [] -> []
            newConstrs -> [Main newConstrs]

instance Optimizable PrologConstraint where
    optimize p@(PredCall name args) =
        case name of
            "always" -> []
            _ -> [p]
    optimize p@(PrologOp op e1 e2) =
        case (op,e1,e2) of
            ("=", PrologVar v1, PrologVar v2) | v1 == v2 -> []
            _ -> [p]
    optimize (Condition cs1 cs2) = [Condition (concatMap optimize cs1) (concatMap optimize cs2)]
    optimize (Disjunction cs1 cs2) = [Disjunction (concatMap optimize cs1) (concatMap optimize cs2)]
    optimize (Conjunction cs1 cs2) = [Conjunction (concatMap optimize cs1) (concatMap optimize cs2)]

