{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Enki.Compiler.CodeGen where

import Control.Lens
import Control.Arrow ((***))
import Control.Monad.Trans.State.Lazy

import Data.Char
import Data.Either
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Enki.Types
import Enki.Compiler.Types

import System.IO.Unsafe

data CodeGenEnv = CodeGenEnv
    { _freshCounter :: Integer }
    deriving (Eq, Show)
makeLenses ''CodeGenEnv

newCodeGenEnv :: CodeGenEnv
newCodeGenEnv = CodeGenEnv 0

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
    i <- freshCounter <<+= 1
    pure $ "Temp" ++ show i

resetCounter :: Monad m => StateT CodeGenEnv m ()
resetCounter = freshCounter .= 0

destructComp :: Computation -> ([PrologConstraint], [PrologExpr])
destructComp (Computation cs res) = (cs, [res])
destructComp (ConstraintComp cs) = (cs, [])

concatComps :: [Computation] -> ([PrologConstraint], [PrologExpr])
concatComps = (concat *** concat) . unzip . map destructComp

safeInit [] = []
safeInit xs@(_:_) = init xs

initComps :: [Computation] -> ([PrologConstraint], [PrologExpr])
initComps = (concatMap safeInit *** concat) . unzip . map destructComp

genFuncCall :: Monad m => TypedDef -> Map String Computation -> String -> StateT CodeGenEnv m ([PrologConstraint], PrologExpr)
genFuncCall def paramVals resName = do
    let comps = map doLookup $ vars $ defId def
    let name  = idToName $ defId def

    let (constraints, params) = if defId def == Comp [S "not", V "G"] then initComps comps else concatComps comps

    pure $ case def of
        TypedConstructor _ _ -> (constraints, PrologFunctor name params)

        TypedFunc _ _ _ _ -> (constraints ++ [PredCall name $ params ++ [PrologVar resName]], PrologVar resName)

        -- The result of this is not really used, but we return it anyway for consistency's sake
        TypedRule _ _ _ -> (constraints ++ [PredCall name params], PrologFunctor name params)

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
prologOp "="  = "="
prologOp "!="  = "\\="
prologOp str  = error $ "Unknown operator: '" ++ str ++ "'"

genFuncCallWith (FuncCall def varMap) resName = do
    params <- mapM (fmap head . codeGen) varMap

    (constrs, res) <- genFuncCall def params resName

    pure [Computation constrs res]

instance CodeGen TypedId Computation where
    codeGen (StringVal v)         = pure [Computation [] $ PrologAtom v]
    codeGen (IntVal i)            = pure [Computation [] $ PrologInt i]
    codeGen (VarVal v)            = pure [Computation [] $ PrologVar v]
    codeGen (FuncRef func _ freeVars boundParams) = do
        let boundVarNames = vars (defId func) \\ Map.keys freeVars
        let boundVars = Map.fromList (zip boundVarNames (map VarVal boundParams))
        let varMap = Map.union freeVars boundVars

        resName <- newVar
        gen <- genFuncCallWith (FuncCall func varMap) resName

        let freeVarNames = concatMap typedIdVars $ Map.elems freeVars

        case gen of
            [Computation constrs res] -> do
                let newRes =
                        case last constrs of
                            PredCall name params ->
                                if isFuncLike func then
                                    PrologLambda freeVarNames (boundParams ++ [resName]) $ PredCall name params
                                else
                                    PrologLambda freeVarNames boundParams $ PredCall name params

                pure $ [Computation (init constrs) newRes]

    codeGen f@(FuncCall def varMap) = do
        resName <- newVar
        genFuncCallWith f resName

    codeGen (BinOp op opType a b) = do
        (aComp, bComp) <-
            case (a, op, b) of
                (VarVal varName, "=", f@(FuncCall _ _)) -> do
                    aRes <- codeGen a
                    bRes <- genFuncCallWith f varName

                    pure (aRes, bRes)

                -- TODO: Maybe just do an AST manipulation so it always looks like X = f(Ys) so we only need one case
                (f@(FuncCall _ _), "=", VarVal varName) -> do
                    aRes <- genFuncCallWith f varName
                    bRes <- codeGen b

                    pure (aRes, bRes)

                (_,_,_) -> (,) <$> codeGen a <*> codeGen b

        let [Computation aConstrs aRes] = aComp
        let [Computation bConstrs bRes] = bComp

        if opType == Void then
            pure [ConstraintComp $ aConstrs ++ bConstrs ++ [PrologOp (prologOp op) aRes bRes]]
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

    codeGen (TypedConstraints []) = pure [ConstraintComp []]
    codeGen (TypedConstraints (c:cs)) = do
        first <- codeGen c
        restGen <- codeGen $ TypedConstraints cs

        let (ctr, _) = concatComps first
        let (rest, _) = concatComps restGen

        pure [ConstraintComp $ ctr ++ rest]

    codeGen (TypedWhen cond (TypedConstraints [])) = codeGen cond
    codeGen (TypedWhen cond body) = do
        condComp <- codeGen cond
        bodyComp <- codeGen body

        case (condComp,bodyComp) of
            ([ConstraintComp condCs], [ConstraintComp bodyCs]) -> pure [ConstraintComp [Condition condCs bodyCs]]

instance CodeGen TypedDef PrologDef where
    codeGen (TypedFunc funcId funcType constr expr) = do
        constrComp <- codeGen constr
        eComp <- codeGen expr

        case (constrComp, eComp) of
            ([ConstraintComp cs], [Computation es res]) ->
                pure [Predicate (show funcType) (idToName funcId) (vars funcId ++ [prettyExpr res]) $ cs ++ es]

    codeGen (TypedRule ruleId ruleType constr) = do
        constrComp <- codeGen constr

        case constrComp of
            [ConstraintComp cs] -> pure [Predicate (show ruleType) (idToName ruleId) (vars ruleId) cs]

    codeGen (TypedData _ _) = pure []

    codeGen (TypedModule _ defs) = concat <$> mapM codeGen defs

    codeGen (TypedExec constr) = do
        constrComp <- codeGen constr

        case constrComp of
            [ConstraintComp cs] -> pure [Main cs]

    codeGen e = error $ show e

class PrettyPrint a where
    prettyPrint :: a -> [String]

prettyExpr :: PrettyPrint a => a -> String
prettyExpr = head . prettyPrint

unquote = reverse . dropWhile (== '\"') . reverse . dropWhile (== '\"')

instance PrettyPrint PrologExpr where
    prettyPrint (PrologInt i)               = [show i]
    prettyPrint (PrologAtom str)            = ["'" ++ unquote str ++ "'"]
    prettyPrint (PrologVar str)             = [str]
    prettyPrint (PrologOpExpr op e1 e2)     = ["(" ++ prettyExpr e1 ++ " " ++ op ++ " " ++ prettyExpr e2 ++ ")"]
    prettyPrint (PrologFunctor name params)
        | null params = [name]
        | otherwise = [name ++ "(" ++ intercalate "," (map prettyExpr params) ++ ")"]
    prettyPrint (PrologLambda free bound body) =
        [curryBodies free bound body]

curryBodies _ [] body = head $ prettyPrint body
curryBodies free (a:b:c:rest) body =
    "{" ++ intercalate "," free ++ "}/" ++
    "[" ++ a ++ "," ++ resName ++ "]" ++
    ">>(" ++ resName ++ " = (" ++ curryBodies (free ++ [a]) (b:c:rest) body ++ "))"
    where
        resName = a ++ "Result"
curryBodies free (bound:rest) body =
    "{" ++ intercalate "," free ++ "}/" ++
    "[" ++ bound ++ "]" ++
    ">>(" ++ curryBodies (free ++ [bound]) rest body ++ ")"

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
        mapConj cond ++ ["    ,"] ++ mapConj body
    prettyPrint (Disjunction a [])    = mapConj a
    prettyPrint (Disjunction a b)     =
        ["("] ++ mapConj a ++ ["    ;"] ++ mapConj b ++ [")"]

instance PrettyPrint PrologDef where
    prettyPrint (Predicate typeStr name [] []) = ["% " ++ typeStr, name ++ "."]
    prettyPrint (Predicate typeStr name params []) = ["% " ++ typeStr, name ++ "(" ++ intercalate "," params ++ ")."]
    prettyPrint (Predicate typeStr name [] constrs) =
        ["% " ++ typeStr, name ++ " :-"] ++
        placeLast "." (mapConj constrs)
    prettyPrint (Predicate typeStr name params constrs) =
        ["% " ++ typeStr, name ++ "(" ++ intercalate "," params ++ ") :-"] ++
        placeLast "." (mapConj constrs)

findMains :: [PrologDef] -> ([PrologConstraint], [PrologDef])
findMains = (\(a, b) -> (concat a, b)) . partitionEithers . map go
    where
        go (Main c) = Left c
        go def      = Right def

makeMain :: [PrologDef] -> ([String], [PrologDef])
makeMain defs = (mainStr, rest)
    where
        (mains, rest) = findMains defs
        mainStr =
            case mains of
                [] -> []
                _  -> [":- initialization(main, main).\n", "main(Argv) :-"] ++ placeLast "." (mapConj mains)

instance PrettyPrint PrologFile where
    prettyPrint (PrologFile prologLibrary defs) =
        [header ++ "\n", "\n" ++ prologLibrary ++ "\n"] ++
        main ++
        concat (placeSep "\n" (map prettyPrint rest))
        where
            (main, rest) = makeMain defs

header = "#!/usr/bin/env swipl\n\n" ++
         ":- use_module(library(clpfd)).\n\n" ++
         ":- use_module(library(yall)).\n\n" ++
         ":- style_check(-singleton).\n" ++
         ":- style_check(-no_effect).\n" ++
         ":- style_check(-var_branches).\n" ++
         ":- style_check(-discontiguous).\n" ++
         ":- style_check(-charset)."

