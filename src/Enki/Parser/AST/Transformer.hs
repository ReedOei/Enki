{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Enki.Parser.AST.Transformer where

import Control.Monad
import Control.Lens (makeLenses, (^.), over)
import Control.Monad.Trans.State.Lazy

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import Enki.Parser.AST
import Enki.Types

import System.IO.Unsafe

data TransEnv = TransEnv
    { _freshCounter :: Integer
    , _aliases :: Map Id Id }
    deriving (Eq, Show)
makeLenses ''TransEnv

type Transform m a = StateT TransEnv m a

data Context = InFunc | InRule | InField | In
    deriving (Eq, Show)

newTransEnv :: TransEnv
newTransEnv = TransEnv 0 Map.empty

runTransform :: [Def] -> [Def]
runTransform defs = filter keep $ evalState (mapM transform defs) newTransEnv
    where
        keep (Alias _ _)  = False
        keep _            = True

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

    transform alias@(Alias key val) = do
        modify $ over aliases $ Map.insert key val
        pure alias

replacePlaceholders :: Monad m => Id -> Transform m Id
replacePlaceholders (Comp ids) = Comp <$> go ids
    where
        go [] = pure []
        go (V "_":rest) = do
            newParam <- freshId
            (V newParam :) <$> go rest
        go (id:rest) = (id :) <$> go rest
replacePlaceholders id = pure id

applyMapping :: Map String Id -> Id -> Id
applyMapping mapping s@(S _) = s
applyMapping mapping i@(I _) = i
applyMapping mapping v@(V x) = fromMaybe v $ Map.lookup x mapping
applyMapping mapping (Comp ids) = Comp $ map (applyMapping mapping) ids
applyMapping mapping (DefRef n id) = DefRef n $ applyMapping mapping id

applyAlias :: Id -> Map Id Id -> Maybe Id
applyAlias id aliasMap =
    case mapMaybe (\(key, val) -> (val,) <$> unifyIds id key) $ Map.toList aliasMap of
        [] -> Nothing
        ((val,mapping):_) -> Just $ applyMapping mapping val

repeatAlias :: Id -> Map Id Id -> Id
repeatAlias id aliasMap =
    case applyAlias id aliasMap of
        Nothing -> id
        Just newId -> repeatAlias newId aliasMap

handleAliases :: Monad m => Id -> Transform m Id
handleAliases id = do
    aliasMap <- (^. aliases) <$> get
    pure $ repeatAlias id aliasMap

instance Transformable Id where
    transform id = innerTransform =<< handleAliases id
        where
            innerTransform c@(Comp ids) = do
                let paramCount = fromIntegral $ length $ filter (== (V "_")) ids
                -- TODO: Disallow nested funcids
                if paramCount > 0 then
                    DefRef paramCount <$> replacePlaceholders c
                else
                    case ids of
                        [inner@(Comp innerIds)] -> transform inner
                        _ -> Comp <$> mapM transform ids

            innerTransform id = handleAliases id

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
    transform tn@(TypeName _) = do
        newT <- applyTypeAlias tn
        case newT of
            TypeName types -> TypeName <$> mapM transform types
            other -> transform other
        where
            applyTypeAlias tn'@(TypeName types) = idToType <$> handleAliases (typeToId tn')
            applyTypeAlias t = pure t
    transform t = pure t

instance Transformable Constructor where
    transform (Constructor id fields) = Constructor <$> transform id <*> mapM transform fields

