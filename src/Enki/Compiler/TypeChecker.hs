{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Enki.Compiler.TypeChecker where

import Control.Lens hiding (get)
import Control.Monad ((<=<), zipWithM_, zipWithM)
import Control.Monad.Trans.State.Lazy

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Enki.Types
import Enki.Parser.AST
import Enki.Compiler.Types
import Enki.Util

import System.IO.Unsafe

data Environment = Environment
    { _typeEnv :: Map String Type
    , _typeVars :: Map String Type
    , _curDef :: Maybe Def
    , _funcEnv :: [TypedDef]
    , _freshCounter :: Integer }
    deriving (Eq, Show)
makeLenses ''Environment

writelnBuiltIn :: TypedDef
writelnBuiltIn = TypedRule (Comp [S "writeln", V "Str"]) EnkiString (TypedConstraints [])

termToAtom :: TypedDef
termToAtom = TypedFunc (Comp [S "term_to_atom", V "I"]) (FuncType (Any "ANYTHING") EnkiString) (TypedConstraints []) (TypedExpr (StringVal "BUILTIN"))

prologNot :: TypedDef
prologNot = TypedRule (Comp [S "not", V "G"]) (Any "ANYTHING") (TypedConstraints [])

newEnv :: Environment
newEnv = Environment Map.empty Map.empty Nothing [writelnBuiltIn, termToAtom, prologNot] 0

defineNew :: Monad m => TypedDef -> StateT Environment m TypedDef
defineNew def = do
    modify $ over funcEnv (def:)
    -- Clear out the typing environment, because we have just finished defining this function
    modify $ set typeEnv Map.empty
    modify $ set typeVars Map.empty
    modify $ set curDef Nothing
    pure def

freshType :: Monad m => StateT Environment m Type
freshType = do
    i <- (^.freshCounter) <$> get
    modify $ over freshCounter (+1)
    pure $ Any $ "T" ++ show i

resolveType :: Monad m => Type -> StateT Environment m Type
resolveType t@(Any name) = do
    env <- (^.typeVars) <$> get
    case Map.lookup name env of
        Nothing -> pure t
        Just (Any newName) | newName == name -> pure t
        Just mapping -> resolveType mapping
resolveType (FuncType t1 t2) = FuncType <$> resolveType t1 <*> resolveType t2
resolveType (RuleType t1 t2) = RuleType <$> resolveType t1 <*> resolveType t2
resolveType (DataType t1 t2) = DataType <$> resolveType t1 <*> resolveType t2
resolveType (TypeName types) = TypeName <$> mapM resolveType types
resolveType t = pure t

resolveDefType (TypedFunc id t constr expr) = TypedFunc <$> pure id <*> resolveType t <*> pure constr <*> pure expr
resolveDefType (TypedRule id t constr) = TypedRule <$> pure id <*> resolveType t <*> pure constr
resolveDefType (TypedConstructor id t) = TypedConstructor <$> pure id <*> resolveType t
resolveDefType def = pure def

lookupType :: Monad m => String -> StateT Environment m Type
lookupType name = do
    env <- (^.typeEnv) <$> get
    case Map.lookup name env of
        Nothing -> do
            t <- freshType
            updateType name t
            resolveType t
        Just t -> resolveType t

updateType :: Monad m => String -> Type -> StateT Environment m ()
updateType str newType = modify $ over typeEnv $ Map.insert str newType

unifyTypeVars :: Monad m => Type -> Type -> StateT Environment m ()
unifyTypeVars (Any old) new = modify $ over typeVars $ Map.insert old new
unifyTypeVars (FuncType t1 t2) (FuncType t3 t4) = do
    unifyTypeVars t1 t3
    unifyTypeVars t2 t4
unifyTypeVars (RuleType t1 t2) (RuleType t3 t4) = do
    unifyTypeVars t1 t3
    unifyTypeVars t2 t4
unifyTypeVars (DataType t1 t2) (DataType t3 t4) = do
    unifyTypeVars t1 t3
    unifyTypeVars t2 t4
unifyTypeVars (TypeName types1) (TypeName types2) = zipWithM_ unifyTypeVars types1 types2
unifyTypeVars old new = pure ()

updateAllType :: Monad m => Type -> Type -> StateT Environment m ()
updateAllType oldType newType = do
    unifyTypeVars oldType newType

    env <- (^.typeEnv) <$> get
    newEnv <- mapM go env
    modify $ set typeEnv newEnv

    where
        go t
            | t == oldType = resolveType newType
            | otherwise = resolveType t

makeFuncType :: Monad m => [String] -> TypedExpr -> StateT Environment m Type
makeFuncType vars tExpr = do
    types <- mapM lookupType vars

    retType <- typeOf tExpr
    pure $ foldr1 FuncType $ types ++ [retType]

makeRuleType :: Monad m => [String] -> StateT Environment m Type
makeRuleType vars = do
    types <- mapM lookupType vars

    pure $ foldr1 RuleType types

-- The first parameter is the id we wish to match (e.g., the function call expression),
-- the second is the function we are checking if it matches
unifyIds :: Id -> Id -> Maybe (Map String Id)
unifyIds (S s1) (S s2)       = if s1 == s2 then Just Map.empty else Nothing
unifyIds (I i1) (I i2)       = if i1 == i2 then Just Map.empty else Nothing
unifyIds (B b1) (B b2)       = if b1 == b2 then Just Map.empty else Nothing
unifyIds id (V varName)      = Just $ Map.fromList [(varName, id)]
unifyIds (Comp []) (Comp []) = Just Map.empty
unifyIds id1 (Comp [id2])    = unifyIds id1 id2
unifyIds (Comp [id1]) id2    = unifyIds id1 id2
unifyIds (Comp (id1:ids1)) (Comp (id2:ids2)) = Map.union <$> unifyIds id1 id2 <*> unifyIds (Comp ids1) (Comp ids2)
unifyIds _ _ = Nothing

types :: Type -> [Type]
types (FuncType t1 t2) = t1 : types t2
types (RuleType t1 t2) = t1 : types t2
types (DataType t1 t2) = t1 : types t2
types t = [t]

returnType :: Type -> Type
returnType (FuncType t1 t2) = returnType' t2
returnType (DataType t1 t2) = returnType' t2
returnType (RuleType _ _)   = Void
returnType t                = t

returnType' :: Type -> Type
returnType' (FuncType t1 t2) = returnType' t2
returnType' (DataType t1 t2) = returnType' t2
returnType' t                = t

freshDefType :: Monad m => TypedDef -> StateT Environment m TypedDef
freshDefType (TypedFunc id t constr expr) = TypedFunc <$> pure id <*> freshTypeVars t <*> pure constr <*> pure expr
freshDefType (TypedRule id t constr) = TypedRule <$> pure id <*> freshTypeVars t <*> pure constr
freshDefType (TypedConstructor id t) = TypedConstructor <$> pure id <*> freshTypeVars t
freshDefType def = error $ "Cannot use def as a function call (freshDefType): " ++ show def

funcParamTypes :: TypedDef -> Map String Type
funcParamTypes (TypedFunc funcId funcType _ _)      = Map.fromList $ zip (vars funcId) $ types funcType
funcParamTypes (TypedRule ruleId ruleType _)        = Map.fromList $ zip (vars ruleId) $ types ruleType
funcParamTypes (TypedConstructor constrId dataType) = Map.fromList $ zip (vars constrId) $ types dataType
funcParamTypes _                                    = Map.empty

pairWithParamType :: Monad m => TypedDef -> Map String Id -> StateT Environment m (Map String (Type, Id), TypedDef)
pairWithParamType def idMap = do
    newDef <- freshDefType def
    pure (Map.intersectionWith (,) (funcParamTypes newDef) idMap, newDef)

join :: Type -> Type -> Maybe Type
join x (Any _) = Just x
join (Any _) x = Just x
join (TypeName parts1) (TypeName parts2) = TypeName <$> zipWithM join parts1 parts2
join x y
    | x == y = Just x
    | otherwise = Nothing

unifyFunc :: Monad m => Id -> TypedDef -> StateT Environment m (Maybe (Map String (Type, Id), TypedDef))
unifyFunc id def =
    case unifyIds id (defId def) of
        Nothing -> pure Nothing
        Just idMap -> Just <$> pairWithParamType def idMap

unifyTypes :: Monad m => Type -> [Type] -> StateT Environment m ()
unifyTypes target = mapM_ (`updateAllType` target)

joinTypes :: Monad m => Type -> Type -> StateT Environment m Bool
joinTypes t1 t2 =
    case join t1 t2 of
        Nothing -> pure False
        Just newT -> do
            unifyTypes newT [t1,t2]
            pure True

unify :: Monad m => Type -> TypedId -> StateT Environment m Bool
unify t (StringVal _) = joinTypes t EnkiString
unify t (IntVal _)    = joinTypes t EnkiInt
unify t (BoolVal _)   = joinTypes t EnkiBool
unify t (VarVal str)  = do
    curT <- lookupType str
    joinTypes t curT
unify t (BinOp _ opType _ _) = joinTypes t opType
unify t (FuncCall def _) = do
    funcType <- typeOf def
    vars <- (^.typeVars) <$> get
    env <- (^.typeEnv) <$> get

    joinTypes t $ returnType funcType

inferAndUnify :: Monad m => (Type, Id) -> StateT Environment m Bool
inferAndUnify (t, id) = do
    newT <- resolveType t
    inferred <- infer id
    res <- unify newT inferred
    if not res then do
        vars <- (^.typeVars) <$> get
        env <- (^.typeEnv) <$> get
        error $ "Inferred\n" ++ show inferred ++ "\nfor\n" ++ show id ++ "\nbut failed to unify this with\n" ++ show t ++ "\nin the environment\n" ++ show env ++ "\nwith type vars\n" ++ show vars
    else
        pure res

unifyAll :: Monad m => [(Type, Id)] -> StateT Environment m ()
unifyAll pairs = do
    res <- mapM inferAndUnify pairs
    if not $ and res then
        error $ "Failed to unify parameters: " ++ show pairs ++ " " ++ show res
    else
        pure ()

freshTypeVars :: Monad m => Type -> StateT Environment m Type
freshTypeVars t = do
    -- Save state so we can restore it later
    origVars <- (^.typeVars) <$> get

    -- Clear state so that we end up with fresh variables
    modify $ set typeVars Map.empty

    newT <- go t

    -- Restore the state
    modify $ over typeVars $ const origVars

    pure newT

    where
        go (Any name) = do
            env <- (^.typeVars) <$> get
            case Map.lookup name env of
                Nothing -> do
                    newType <- freshType
                    let (Any newName) = newType
                    -- Add the new type to the environment so later lookups will get the same thing
                    modify $ over typeVars $ Map.insert name $ Any newName
                    pure newType
                Just t -> pure t
        go (FuncType t1 t2) = FuncType <$> go t1 <*> go t2
        go (RuleType t1 t2) = RuleType <$> go t1 <*> go t2
        go (DataType t1 t2) = DataType <$> go t1 <*> go t2
        go (TypeName types) = TypeName <$> mapM go types
        go t = pure t

-- | Makes a typeddef from a def, with an empty body. Exists to allow for recursive calls.
makeTempTyped :: Monad m => Def -> StateT Environment m TypedDef
makeTempTyped (Func id _ _) = do
    paramTypes <- mapM lookupType $ vars id
    retType <- freshType
    pure $ TypedFunc id (foldr1 FuncType (paramTypes ++ [retType])) (TypedConstraints []) (TypedExpr (VarVal "DUMMY_VAR"))
makeTempTyped (Rule id _) = do
    paramTypes <- mapM lookupType $ vars id
    pure $ TypedRule id (foldr1 RuleType paramTypes) (TypedConstraints [])

showMapping :: (String, (Type, Id)) -> String
showMapping (name, (t, id)) = name ++ " -> " ++ show id ++ " : " ++ show t

showFuncMatch :: (Map String (Type, Id), TypedDef) -> String
showFuncMatch (mapping, def) = "Match: " ++ show (defId def) ++ ". Parameters:\n" ++
    intercalate "\n" (map (indent 1 . showMapping) (Map.toList mapping))

findCall :: Monad m => Id -> StateT Environment m (Maybe (Map String (Type, Id), TypedDef))
findCall id = do
    funcs <- (^.funcEnv) <$> get
    maybeDef <- (^.curDef) <$> get
    curTyped <- case maybeDef of
                    Nothing -> pure []
                    Just def -> (:[]) <$> makeTempTyped def

    possible <- catMaybes <$> mapM (unifyFunc id) (curTyped ++ funcs)

    case possible of
        [] -> pure Nothing
        [match] -> pure $ Just match
        _ -> error $ "The function call " ++ show id ++ " is ambiguous. It could match any of:\n" ++
            intercalate "\n" (map showFuncMatch possible)

class Typeable a where
    getType :: Monad m => a -> StateT Environment m Type

class Inferable a b | a -> b where
    infer :: Monad m => a -> StateT Environment m b

instance Typeable TypedDef where
    getType (TypedFunc _ t _ _)    = pure t
    getType (TypedRule _ t _)      = pure t
    getType (TypedConstructor _ t) = pure t
    getType (TypedExec constrs)    = pure Void
    getType (TypedModule _ _)      = pure Void

instance Typeable TypedId where
    getType (StringVal _)    = pure EnkiString
    getType (IntVal _)       = pure EnkiInt
    getType (BoolVal _)      = pure EnkiBool
    getType (VarVal name)    = lookupType name
    getType (FuncCall def _) = returnType <$> getType def
    getType (BinOp _ t _ _)  = pure t

instance Typeable TypedExpr where
    getType (TypedExpr tid) = getType tid

instance Typeable Field where
    getType (Field _ t) = pure t

typeOf :: (Typeable a, Monad m) => a -> StateT Environment m Type
typeOf = resolveType <=< getType

isOp :: String -> Bool
isOp s = s `elem` ["+", "-", "*", "/", "^", "=", "..", ">", ">=", "<", "<="]

type OpConstructor = TypedId -> TypedId -> TypedId

opType :: Monad m => String -> StateT Environment m (Maybe (Type, Type, OpConstructor))
opType "+"  = pure $ Just (EnkiInt, EnkiInt, BinOp "+" EnkiInt)
opType "-"  = pure $ Just (EnkiInt, EnkiInt, BinOp "-" EnkiInt)
opType "*"  = pure $ Just (EnkiInt, EnkiInt, BinOp "*" EnkiInt)
opType "/"  = pure $ Just (EnkiInt, EnkiInt, BinOp "/" EnkiInt)
opType "^"  = pure $ Just (EnkiInt, EnkiInt, BinOp "^" EnkiInt)
opType "<"  = pure $ Just (EnkiInt, EnkiInt, BinOp "<" Void)
opType "<=" = pure $ Just (EnkiInt, EnkiInt, BinOp "<=" Void)
opType ">"  = pure $ Just (EnkiInt, EnkiInt, BinOp ">" Void)
opType ">=" = pure $ Just (EnkiInt, EnkiInt, BinOp ">=" Void)
opType "="  = do
    t <- freshType
    pure $ Just (t,t,BinOp "=" Void)
opType ".." = pure $ Just (EnkiString, EnkiString,
    \x y -> FuncCall (TypedFunc
                        (Comp [S "atom_concat", V "X", V "Y"])
                        (FuncType EnkiString (FuncType EnkiString EnkiString))
                        (TypedConstraints [])
                        (TypedExpr (StringVal "dummy value")))
                     (Map.fromList [("X", x), ("Y", y)]))
opType _ = pure Nothing

inferOp :: Monad m => Id -> StateT Environment m TypedId
inferOp id@(Comp [id1, S op, id2]) = do
    t <- opType op
    case t of
        Nothing -> error $ "Could not resolve function call: " ++ show id
        Just (t1, t2, constr) -> do
            tid1 <- infer id1
            succeed1 <- unify t1 tid1

            tid2 <- infer id2
            succeed2 <- unify t2 tid2

            newT1 <- typeOf tid1
            newT2 <- typeOf tid2

            succeed3 <- joinTypes newT1 newT2

            if not succeed1 then
                error $ "Could not unify (1): " ++ show (t1, tid1)
            else if not succeed2 then
                error $ "Could not unify (2): " ++ show (t2, tid2)
            else if not succeed3 then
                error $ "Could not unify (3): " ++ show (tid1, newT1, tid2, newT2)
            else
                pure $ constr tid1 tid2
inferOp id = error $ "Not an operator expression: " ++ show id

instance Inferable Id TypedId where
    infer id@(S str) = do
        res <- findCall $ Comp [id]
        case res of
            Nothing -> pure $ StringVal str
            Just (typeMap, func) -> do
                unifyAll $ Map.elems typeMap
                params <- mapM (infer . snd) typeMap
                newDef <- resolveDefType func
                pure $ FuncCall newDef params
    infer (I i) = pure $ IntVal i
    infer (B b) = pure $ BoolVal b
    infer (V str) = do
        -- Do this to generate a new type if necessary
        lookupType str
        pure $ VarVal str
    infer id@(Comp ids) = do
        res <- findCall id

        case res of
            Nothing ->
                case ids of
                    [inner] -> infer inner
                    _       -> inferOp id
            Just (typeMap, func) -> do
                unifyAll $ Map.elems typeMap
                params <- mapM (infer . snd) typeMap

                newDef <- resolveDefType func
                pure $ FuncCall newDef params

instance Inferable Expr TypedExpr where
    infer (Expr id) = TypedExpr <$> infer id

instance Inferable Constraint TypedConstraint where
    infer (Constraint id) = TypedConstraint <$> infer id
    infer (Constraints cs) = TypedConstraints <$> mapM infer cs
    infer (When cond body) = TypedWhen <$> infer cond <*> infer body

makeConstructor :: Monad m => Type -> Constructor -> StateT Environment m TypedDef
makeConstructor resType (Constructor id fields) = do
    fieldTypes <- mapM typeOf fields
    pure $ TypedConstructor id $ foldr DataType resType fieldTypes

instance Inferable Def TypedDef where
    infer def@(Func id constr expr) = do
        modify $ set curDef $ Just def

        -- Add types for each parameter of the function
        mapM_ (infer . V) $ vars id

        tConstr <- infer constr
        tExpr <- infer expr

        funcType <- makeFuncType (vars id) tExpr

        defineNew $ TypedFunc id funcType tConstr tExpr

    infer def@(Rule id constr) = do
        modify $ set curDef $ Just def

        mapM_ (infer . V) $ vars id

        tConstr <- infer constr
        ruleType <- makeRuleType $ vars id

        defineNew $ TypedRule id ruleType tConstr

    infer (Data id constrs) = do
        let resType = TypeName $ makeTypeName id
        TypedData id <$> mapM (defineNew <=< makeConstructor resType) constrs

    infer (Exec constr) = TypedExec <$> infer constr

    infer (Module name defs) = TypedModule name <$> mapM infer defs

    infer e = error $ show e

instance Inferable a b => Inferable [a] [b] where
    infer = mapM infer

