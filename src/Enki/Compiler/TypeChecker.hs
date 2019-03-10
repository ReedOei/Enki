{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}

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

freshType :: Monad m => StateT Environment m Type
freshType = do
    i <- (^.freshCounter) <$> get
    modify $ over freshCounter (+1)
    pure $ Any $ "t" ++ show i

lookupType :: Monad m => String -> StateT Environment m (Maybe Type)
lookupType name = do
    env <- (^.typeEnv) <$> get
    pure $ Map.lookup name env

unifyIds :: Id -> Id -> Maybe [(String, Id)]
unifyIds (S s1) (S s2)       = if s1 == s2 then Just [] else Nothing
unifyIds (I i1) (I i2)       = if i1 == i2 then Just [] else Nothing
unifyIds (B b1) (B b2)       = if b1 == b2 then Just [] else Nothing
unifyIds id (V varName)      = Just [(varName, id)]
unifyIds (Comp []) (Comp []) = Just []
unifyIds id1 (Comp [id2]) = unifyIds id1 id2
unifyIds (Comp (id1:ids1)) (Comp (id2:ids2)) = (++) <$> unifyIds id1 id2 <*> unifyIds (Comp ids1) (Comp ids2)

unifyFunc :: Id -> TypedDef -> Maybe (Map String TypedId, TypedDef)
unifyFunc id (TypedFunc funcId funcType constr tExpr) = Nothing
unifyFunc id (TypedRule ruleId ruleType constr) = Nothing
unifyFunc id (TypedData dataId dataType constructors) = Nothing
unifyFunc id _ = Nothing

findCall :: Monad m => Id -> StateT Environment m (Maybe (Map String TypedId, TypedDef))
findCall id = do
    funcs <- (^.funcEnv) <$> get
    pure $ listToMaybe $ catMaybes $ map (unifyFunc id) funcs

class Typeable a where
    typeOf :: a -> Type

class Inferable a b | a -> b where
    infer :: Monad m => a -> StateT Environment m b

instance Typeable TypedId where
    typeOf (StringVal _) = EnkiString
    typeOf (IntVal _)    = EnkiInt
    typeOf (BoolVal _)   = EnkiBool
    typeOf (VarVal t _)  = t
    typeOf (CompVal t _) = t

instance Inferable Id TypedId where
    infer (S str) = pure $ StringVal str
    infer (I i) = pure $ IntVal i
    infer (B b) = pure $ BoolVal b
    infer (V str) = do
        t <- lookupType str
        typeof <- case t of
            Nothing -> freshType
            Just tType -> pure tType
        pure $ VarVal typeof str

instance Inferable Expr TypedExpr where
    infer (Expr id) = TypedExpr <$> infer id

instance Inferable Constraint TypedConstraint where


instance Inferable Def TypedDef where
    infer (Func id constr expr) = undefined

-- data TypedDef = TypedFunc Id TypedConstraint TypedExpr
--               | TypedRule Id TypedConstraint
--               | TypedData Id [TypedConstructor]
--               | TypedExec TypedConstraint
--               | TypedModule String [TypedDef]

