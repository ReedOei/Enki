{-# LANGUAGE TemplateHaskell #-}

module Enki.Parser.AST.Transformer where

import Control.Monad
import Control.Lens (makeLenses, (^.), over)
import Control.Monad.Trans.State.Lazy

import Enki.Parser.AST
import Enki.Types

data TransEnv = TransEnv
    { _freshCounter :: Integer }
    deriving (Eq, Show)
makeLenses ''TransEnv

type Transform m a = StateT TransEnv m a

data Context = InFunc | InRule | InField | In
    deriving (Eq, Show)

newTransEnv :: TransEnv
newTransEnv = TransEnv 0

runTransform :: [Def] -> [Def]
runTransform defs = evalState (mapM transform defs) newTransEnv

freshId :: Monad m => Transform m String
freshId = do
    val <- (^.freshCounter) <$> get
    modify $ over freshCounter (+1)
    pure $ "_" ++ show val

class Transformable a where
    transform :: Monad m => a -> Transform m a

instance Transformable Def where
    transform f@(Func id constr expr) =
        Func <$> transform id <*> transform constr <*> transform expr
    transform r@(Rule id constr) =
        Rule <$> transform id <*> transform constr
    transform d@(Data id constructors) =
        Data <$> transform id <*> mapM transform constructors
    transform e@(Exec constr) =
        Exec <$> transform constr

    -- TODO: Allow name transformations
    transform m@(Module name defs) =
        Module name <$> mapM transform defs
    transform noImport@(NoImport _) = pure noImport

replacePlaceholders :: Monad m => Id -> Transform m Id
replacePlaceholders (Comp ids) = Comp <$> go ids
    where
        go [] = pure []
        go (V "_":rest) = do
            newParam <- freshId
            (V newParam :) <$> go rest
        go (id:rest) = (id :) <$> go rest
replacePlaceholders id = pure id

instance Transformable Id where
    transform c@(Comp ids)
        -- TODO: Disallow nested funcids
        | paramCount > 0 = DefRef paramCount <$> replacePlaceholders c
        | otherwise =
            case ids of
                [inner@(Comp innerIds)] -> transform inner
                _ -> Comp <$> mapM transform ids
        where paramCount = fromIntegral $ length $ filter (== (V "_")) ids

    transform id = pure id

instance Transformable Expr where
    transform (Expr exprId) = Expr <$> transform exprId

instance Transformable Constraint where
    transform (Constraint cid) = Constraint <$> transform cid
    transform (Constraints cs) = Constraints <$> mapM transform cs
    transform (When cond body) = When <$> transform cond <*> transform body

instance Transformable Field where
    transform (Field id typ) = Field <$> transform id <*> transform typ

instance Transformable Type where
    transform (FuncType t1 t2) = FuncType <$> transform t1 <*> transform t2
    transform (RuleType t1 t2) = RuleType <$> transform t1 <*> transform t2
    transform (DataType t1 t2) = DataType <$> transform t1 <*> transform t2
    transform (TypeName types) = TypeName <$> mapM transform types
    transform t = pure t

instance Transformable Constructor where
    transform (Constructor id fields) = Constructor <$> transform id <*> mapM transform fields

