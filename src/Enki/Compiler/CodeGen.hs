{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Enki.Compiler.CodeGen where

import Control.Lens
import Control.Monad.Trans.State.Lazy

import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Enki.Types
import Enki.Compiler.Types

data CodeGenEnv = CodeGenEnv
    { _freshCounter :: Integer }
    deriving (Eq, Show)
makeLenses ''CodeGenEnv

newCodeGenEnv :: CodeGenEnv
newCodeGenEnv = CodeGenEnv 0

newtype PrologFile = PrologFile { defs :: [PrologDef] }
    deriving (Eq, Show)

data PrologDef = Predicate String [String] [PrologConstraint]
    deriving (Eq, Show)

data PrologExpr = PrologInt Integer
                | PrologAtom String
                | PrologVar String
                | PrologOpExpr String PrologExpr PrologExpr
                | PrologFunctor String [PrologExpr]
    deriving (Eq, Show)

data PrologConstraint = PredCall String [PrologExpr]
                      | PrologOp String PrologExpr PrologExpr
                      | Condition [PrologConstraint] [PrologConstraint]
                      | Disjunction [PrologConstraint] [PrologConstraint]
                      | Conjunction [PrologConstraint] [PrologConstraint]
    deriving (Eq, Show)

data Computation = Computation [PrologConstraint] PrologExpr
                 | ConstraintComp [PrologConstraint]
    deriving (Eq, Show)

class CodeGen a b | a -> b where
    codeGen :: Monad m => a -> StateT CodeGenEnv m [b]

nonVarStrs :: Id -> [String]
nonVarStrs (S str) = [str]
nonVarStrs (Comp []) = []
nonVarStrs (Comp (id:ids)) = nonVarStrs id ++ nonVarStrs (Comp ids)
nonVarStrs _ = []

idToName :: Id -> String
idToName = intercalate "_" . nonVarStrs

newVar :: Monad m => StateT CodeGenEnv m String
newVar = do
    i <- (^.freshCounter) <$> get
    modify $ over freshCounter (+1)
    pure $ "Temp" ++ show i

resetCounter :: Monad m => StateT CodeGenEnv m ()
resetCounter = modify $ over freshCounter (const 0)

destructComp :: Computation -> ([PrologConstraint], [PrologExpr])
destructComp (Computation cs res) = (cs, [res])
destructComp (ConstraintComp cs) = (cs, [])

concatComps :: [Computation] -> ([PrologConstraint], [PrologExpr])
concatComps = over _2 concat . over _1 concat . unzip . map destructComp

genFuncCall :: Monad m => TypedDef -> Map String Computation -> StateT CodeGenEnv m ([PrologConstraint], PrologExpr)
genFuncCall def paramVals = do
    let comps = map doLookup $ vars $ defId def
    let name  = idToName $ defId def

    let (constraints, params) = concatComps comps

    case def of
        TypedConstructor _ _ -> pure (constraints, (PrologFunctor name params))

        TypedFunc _ _ _ _ -> do
            resName <- newVar
            pure (constraints ++ [PredCall name $ params ++ [PrologVar resName]], PrologVar resName)

        -- The result of this is not really used, but we return it anyway for consistency's sake
        TypedRule _ _ _ -> pure (constraints ++ [PredCall name params], last params)

    where
        doLookup paramName =
            case Map.lookup paramName paramVals of
                Nothing -> error $ "Could not find value for parameter '" ++ paramName ++ "'. Have: " ++ show (Map.keys paramVals)
                Just val -> val

eqSign :: Type -> String
eqSign EnkiInt = "#="
eqSign _       = "="

prologOp :: String -> String
prologOp "+"  = "+"
prologOp "-"  = "-"
prologOp "*"  = "*"
prologOp "/"  = "div"
prologOp "^"  = "^"
prologOp ">"  = "#>"
prologOp ">=" = "#>="
prologOp "<"  = "#<"
prologOp "<=" = "#=<"
prologOp str  = error $ "Unknown operator: '" ++ str ++ "'"

instance CodeGen TypedId Computation where
    codeGen (StringVal v)         = pure [Computation [] $ PrologAtom v]
    codeGen (IntVal i)            = pure [Computation [] $ PrologInt i]
    codeGen (BoolVal b)           = pure [Computation [] $ PrologAtom $ map toLower $ show b]
    codeGen (VarVal v)            = pure [Computation [] $ PrologVar v]
    codeGen (FuncCall def varMap) = do
        params <- mapM (fmap head . codeGen) varMap

        (constrs, res) <- genFuncCall def params

        pure [Computation constrs res]

    codeGen (BinOp op opType a b) = do
        aComp <- codeGen a
        bComp <- codeGen b

        let [Computation aConstrs aRes] = aComp
        let [Computation bConstrs bRes] = bComp

        if op == "=" then
            pure [ConstraintComp $ aConstrs ++ bConstrs ++ [PrologOp "=" aRes bRes]]
        else do
            res <- newVar

            let opExpr = PrologOpExpr (prologOp op) aRes bRes

            pure [Computation (aConstrs ++ bConstrs ++ [PrologOp (eqSign opType) (PrologVar res) opExpr]) $ PrologVar res]

instance CodeGen TypedExpr Computation where
    codeGen (TypedExpr tid) = codeGen tid

instance CodeGen TypedConstraint Computation where
    codeGen (TypedConstraint tid) = codeGen tid
    codeGen (TypedConstraints (w@(TypedWhen _ _):cs)) = do
        branchGen <- codeGen w
        restGen <- codeGen $ TypedConstraints cs

        let (branch, _) = concatComps branchGen
        let (rest, _) = concatComps restGen
        pure [ConstraintComp [Disjunction branch rest]]
    codeGen (TypedConstraints cs) = do
        res <- mapM (fmap head . codeGen) cs

        pure [ConstraintComp $ fst $ concatComps res]

    codeGen (TypedWhen cond (TypedConstraints [])) = codeGen cond
    codeGen (TypedWhen cond body) = do
        condComp <- codeGen cond
        bodyComp <- codeGen body

        case (condComp,bodyComp) of
            ([ConstraintComp condCs], [ConstraintComp bodyCs]) -> pure [ConstraintComp [Condition condCs bodyCs]]

instance CodeGen TypedDef PrologDef where
    codeGen (TypedFunc funcId funcType constr expr) = do
        resetCounter
        constrComp <- codeGen constr
        eComp <- codeGen expr

        case (constrComp, eComp) of
            ([ConstraintComp cs], [Computation es res]) ->
                pure [Predicate (idToName funcId) (vars funcId ++ [prettyExpr res]) $ cs ++ es]
    codeGen (TypedRule ruleId ruleType constr) = do
        resetCounter
        constrComp <- codeGen constr

        case constrComp of
            [ConstraintComp cs] ->
                pure [Predicate (idToName ruleId) (vars ruleId) cs]
    codeGen (TypedData _ _) = pure []

class PrettyPrint a where
    prettyPrint :: a -> [String]

prettyExpr :: PrettyPrint a => a -> String
prettyExpr = head . prettyPrint

instance PrettyPrint PrologExpr where
    prettyPrint (PrologInt i)               = [show i]
    prettyPrint (PrologAtom str)            = [str]
    prettyPrint (PrologVar str)             = [str]
    prettyPrint (PrologOpExpr op e1 e2)     = ["(" ++ prettyExpr e1 ++ " " ++ op ++ " " ++ prettyExpr e2 ++ ")"]
    prettyPrint (PrologFunctor name params) = [name ++ "(" ++ intercalate "," (map prettyExpr params) ++ ")"]

placeLast _ []   = []
placeLast str xs = init xs ++ [last xs ++ str]

placeSep _ []   = []
placeSep str xs = map (placeLast str) (init xs) ++ [last xs]

conjunction = placeSep ","

indent = map ("    "++)

mapConj = indent . concat . conjunction . map prettyPrint
-- indentConj = indent . conjunction . concatMap prettyPrint

instance PrettyPrint PrologConstraint where
    prettyPrint (PredCall name exprs) = [name ++ "(" ++ intercalate "," (map prettyExpr exprs) ++ ")"]
    prettyPrint (PrologOp op e1 e2)   = [prettyExpr e1 ++ " " ++ op ++ " " ++ prettyExpr e2]
    prettyPrint (Condition cond body) =
        ["("] ++ mapConj cond ++ ["    ->"] ++ mapConj body ++ [")"]
    prettyPrint (Disjunction a [])    = mapConj a
    prettyPrint (Disjunction a b)     =
        ["("] ++ mapConj a ++ ["    ;"] ++ mapConj b ++ [")"]

instance PrettyPrint PrologDef where
    prettyPrint (Predicate name params []) = [name ++ "(" ++ intercalate "," params ++ ")."]
    prettyPrint (Predicate name params constrs) =
        [name ++ "(" ++ intercalate "," params ++ ")" ++ " :-"] ++
        placeLast "." (mapConj constrs)

instance PrettyPrint PrologFile where
    prettyPrint (PrologFile defs) = [header ++ "\n"] ++ concat (placeSep "\n" (map prettyPrint defs))

header = "#!/usr/bin/env swipl\n\n" ++
         ":- use_module(library(clpfd)).\n\n" ++
         ":- style_check(-singleton).\n" ++
         ":- style_check(-no_effect).\n" ++
         ":- style_check(-var_branches).\n" ++
         ":- style_check(-discontiguous).\n" ++
         ":- style_check(-charset)."

