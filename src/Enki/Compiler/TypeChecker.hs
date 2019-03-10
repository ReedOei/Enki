{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances #-}

module Enki.Compiler.TypeChecker where

import Control.Lens hiding (get,set)
import Control.Monad.Trans.State.Lazy

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Enki.Types
import Enki.Parser.AST
import Enki.Compiler.Types

data Environment = Environment
    { _typeEnv :: Map String Type
    , _funcEnv :: [TypedDef]
    , _freshCounter :: Integer }
    deriving (Eq, Show)
makeLenses ''Environment

newEnv :: Environment
newEnv = Environment Map.empty [] 0

defineNew :: Monad m => TypedDef -> StateT Environment m TypedDef
defineNew def = do
    modify $ over funcEnv (def:)
    -- Clear out the typing environment, because we have just finished defining this function
    modify $ over typeEnv (const Map.empty)
    pure def

freshType :: Monad m => StateT Environment m Type
freshType = do
    i <- (^.freshCounter) <$> get
    modify $ over freshCounter (+1)
    pure $ Any $ "T" ++ show i

lookupType :: Monad m => String -> StateT Environment m Type
lookupType name = do
    env <- (^.typeEnv) <$> get
    case Map.lookup name env of
        Nothing -> do
            t <- freshType
            updateType name t
            pure t
        Just t -> pure t

updateType :: Monad m => String -> Type -> StateT Environment m ()
updateType str newType = modify $ over typeEnv (Map.insert str newType)

updateAllType :: Monad m => Type -> Type -> StateT Environment m ()
updateAllType oldType newType = modify $ over typeEnv (Map.map go)
    where
        go t
            | t == oldType = newType
            | otherwise = t

makeFuncType :: Monad m => [String] -> TypedExpr -> StateT Environment m Type
makeFuncType vars tExpr = do
    types <- mapM lookupType vars

    retType <- typeOf tExpr
    pure $ foldr1 FuncType $ types ++ [retType]

makeRuleType :: Monad m => [String] -> StateT Environment m Type
makeRuleType vars = do
    types <- mapM lookupType vars

    pure $ foldr1 RuleType types

unifyIds :: Id -> Id -> Maybe (Map String Id)
unifyIds (S s1) (S s2)       = if s1 == s2 then Just Map.empty else Nothing
unifyIds (I i1) (I i2)       = if i1 == i2 then Just Map.empty else Nothing
unifyIds (B b1) (B b2)       = if b1 == b2 then Just Map.empty else Nothing
unifyIds id (V varName)      = Just $ Map.fromList [(varName, id)]
unifyIds (Comp []) (Comp []) = Just Map.empty
unifyIds id1 (Comp [id2]) = unifyIds id1 id2
unifyIds (Comp (id1:ids1)) (Comp (id2:ids2)) = Map.union <$> unifyIds id1 id2 <*> unifyIds (Comp ids1) (Comp ids2)

vars :: Id -> [String]
vars (S _) = []
vars (I _) = []
vars (B _) = []
vars (V str) = [str]
vars (Comp []) = []
vars (Comp (id:ids)) = vars id ++ vars (Comp ids)

types :: Type -> [Type]
types (FuncType t1 t2) = t1 : types t2
types (RuleType t1 t2) = t1 : types t2
types (DataType t1 t2) = t1 : types t2
types t = [t]

returnType :: Type -> Type
returnType (FuncType t1 t2) = returnType' t2
returnType (DataType t1 t2) = returnType' t2
returnType _ = Void

returnType' :: Type -> Type
returnType' (FuncType t1 t2) = returnType' t2
returnType' (DataType t1 t2) = returnType' t2
returnType' t = t

funcParamTypes :: TypedDef -> Map String Type
funcParamTypes (TypedFunc funcId funcType _ _) = Map.fromList $ zip (vars funcId) $ types funcType
funcParamTypes (TypedRule ruleId ruleType _) = Map.fromList $ zip (vars ruleId) $ types ruleType
funcParamTypes (TypedData dataId dataType _) = Map.fromList $ zip (vars dataId) $ types dataType
funcParamTypes _ = Map.empty

pairWithParamType :: TypedDef -> Map String Id -> Map String (Type, Id)
pairWithParamType def = Map.intersectionWith (,) (funcParamTypes def)

join :: Type -> Type -> Maybe Type
join x (Any _) = Just x
join (Any _) x = Just x
join x y
    | x == y = Just x
    | otherwise = Nothing

defId :: TypedDef -> Id
defId (TypedFunc funcId _ _ _) = funcId
defId (TypedRule ruleId _ _)   = ruleId
defId (TypedData dataId _ _)   = dataId
defId _                        = Comp []

unifyFunc :: Id -> TypedDef -> Maybe (Map String (Type, Id), TypedDef)
unifyFunc id def = (,def) . pairWithParamType def <$> unifyIds id (defId def)

unifyTypes :: Monad m => Type -> [Type] -> StateT Environment m ()
unifyTypes target types = mapM_ (`updateAllType` target) types

joinTypes :: Monad m => Type -> Type -> StateT Environment m Bool
joinTypes t1 t2 =
    case join t1 t2 of
        Nothing -> pure False
        Just newT -> do
            unifyTypes newT [t1,t2]
            pure True

unify :: Monad m => Type -> TypedId -> StateT Environment m Bool
unify EnkiString (StringVal _) = pure True
unify _ (StringVal _) = pure False
unify EnkiInt (IntVal _) = pure True
unify _ (IntVal _) = pure False
unify EnkiBool (BoolVal _) = pure True
unify _ (BoolVal _) = pure False
unify t (VarVal str) = do
    curT <- lookupType str
    joinTypes t curT
unify t (BinOp _ opType _ _) = joinTypes t opType
unify t (FuncCall def _) = do
    funcType <- typeOf def
    joinTypes t $ returnType funcType

inferAndUnify :: Monad m => (Type, Id) -> StateT Environment m Bool
inferAndUnify (t, id) = do
    inferred <- infer id
    unify t inferred

unifyAll :: Monad m => [(Type, Id)] -> StateT Environment m ()
unifyAll pairs = do
    res <- mapM inferAndUnify pairs
    if not $ or res then
        error $ "Failed to unify parameters: " ++ show pairs
    else
        pure ()

findCall :: Monad m => Id -> StateT Environment m (Maybe (Map String (Type, Id), TypedDef))
findCall id = do
    funcs <- (^.funcEnv) <$> get
    pure $ listToMaybe $ catMaybes $ map (unifyFunc id) funcs

class Typeable a where
    typeOf :: Monad m => a -> StateT Environment m Type

class Inferable a b | a -> b where
    infer :: Monad m => a -> StateT Environment m b

instance Typeable TypedDef where
    typeOf (TypedFunc _ t _ _) = pure t
    typeOf (TypedRule _ t _)   = pure t
    typeOf (TypedData _ t _)   = pure t
    typeOf (TypedExec constrs) = pure Void -- TODO, maybe update this to be the type of the constraints?
    typeOf (TypedModule _ _)   = pure Void

instance Typeable TypedId where
    typeOf (StringVal _)    = pure $ EnkiString
    typeOf (IntVal _)       = pure $ EnkiInt
    typeOf (BoolVal _)      = pure $ EnkiBool
    typeOf (VarVal name)    = lookupType name
    typeOf (FuncCall def _) = returnType <$> typeOf def
    typeOf (BinOp _ t _ _)  = pure t

instance Typeable TypedExpr where
    typeOf (TypedExpr tid) = typeOf tid

isOp :: String -> Bool
isOp s = s `elem` ["+", "-", "*", "/", "^", "=", ".."]

opType :: Monad m => String -> StateT Environment m (Maybe (Type, Type, Type))
opType "+" = pure $ Just (EnkiInt, EnkiInt, EnkiInt)
opType "-" = pure $ Just (EnkiInt, EnkiInt, EnkiInt)
opType "*" = pure $ Just (EnkiInt, EnkiInt, EnkiInt)
opType "/" = pure $ Just (EnkiInt, EnkiInt, EnkiInt)
opType "^" = pure $ Just (EnkiInt, EnkiInt, EnkiInt)
opType "=" = do
    t <- freshType
    pure $ Just (t,t,Void)
opType ".." = pure $ Just (EnkiString, EnkiString,EnkiString)
opType _ = pure Nothing

inferOp :: Monad m => Id -> StateT Environment m TypedId
inferOp id@(Comp [id1, S op, id2]) = do
    t <- opType op
    case t of
        Nothing -> error $ "Could not resolve function call: " ++ show id
        Just (t1, t2, ret) -> do
            tid1 <- infer id1
            unify t1 tid1

            tid2 <- infer id2
            unify t2 tid2

            pure $ BinOp op ret tid1 tid2
inferOp id = error $ "Not an operator expression: " ++ show id

instance Inferable Id TypedId where
    infer (S str) = pure $ StringVal str
    infer (I i) = pure $ IntVal i
    infer (B b) = pure $ BoolVal b
    infer (V str) = do
        -- Do this to generate a new type if necessary
        lookupType str
        pure $ VarVal str
    infer (Comp [id]) = infer id
    infer id@(Comp ids) = do
        res <- findCall id
        case res of
            Nothing -> inferOp id
            Just (typeMap, func) -> do
                unifyAll $ Map.elems typeMap
                params <- mapM (infer . snd) typeMap
                pure $ FuncCall func params

instance Inferable Expr TypedExpr where
    infer (Expr id) = TypedExpr <$> infer id

instance Inferable Constraint TypedConstraint where
    infer (Constraint id) = TypedConstraint <$> infer id
    infer (Constraints cs) = TypedConstraints <$> mapM infer cs
    infer (When cond body) = TypedWhen <$> infer cond <*> infer body

instance Inferable Def TypedDef where
    infer (Func id constr expr) = do
        -- Add types for each parameter of the function
        mapM infer $ map V $ vars id

        tConstr <- infer constr
        tExpr <- infer expr

        funcType <- makeFuncType (vars id) tExpr

        defineNew $ TypedFunc id funcType tConstr tExpr

    infer (Rule id constr) = do
        mapM infer $ map V $ vars id

        tConstr <- infer constr

        ruleType <- makeRuleType $ vars id

        defineNew $ TypedRule id ruleType tConstr

    -- infer (Data id constructors) = defineNew $ TypedData id <$> mapM infer constructors
    infer (Exec constr) = TypedExec <$> infer constr
    infer (Module name defs) = TypedModule name <$> mapM infer defs

instance Inferable a b => Inferable [a] [b] where
    infer as = mapM infer as

