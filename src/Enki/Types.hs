module Enki.Types where

import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

data Id = S String
        | I Integer
        | V String
        | Comp [Id]
        | DefRef Integer Id -- e.g., (pair of _ and _) = DefRef 2 (Comp [S "pair", S "of", V "_1", S "and", V "_2"])
    deriving (Ord, Eq, Show)

data Type = EnkiInt
          | EnkiBool
          | EnkiString
          | Any String
          | Void
          | Unit
          | FuncType Type Type
          | RuleType Type Type
          | DataType Type Type
          | Named String
          | TypeName [Type]
    deriving (Eq, Show)

makeTypeName :: Id -> Type
makeTypeName (S str) = Named str
makeTypeName (V name) = Any name
makeTypeName (Comp [id]) = makeTypeName id
makeTypeName (Comp ids) = TypeName $ map makeTypeName ids

placeHolders :: Id -> [String]
placeHolders (Comp ids) = concatMap placeHolders ids
placeHolders (V name)
    | "_" `isPrefixOf` name = [name]
    | otherwise = []
placeHolders _ = []

isPlaceholder (Comp [id]) = isPlaceholder id
isPlaceholder (V name) = "_" `isPrefixOf` name
isPlaceholder _ = False

unwrapIds :: [Id] -> Id
unwrapIds [x] = x
unwrapIds ids = Comp ids

typeToId :: Type -> Id
typeToId EnkiInt = S "int"
typeToId EnkiString = S "string"
typeToId EnkiBool = S "bool"
typeToId Void = S "void"
typeToId (Named str) = S str
typeToId (Any v) = V v
typeToId (FuncType t1 t2) = Comp [typeToId t1, S "->", typeToId t2]
typeToId (RuleType t1 t2) = Comp [typeToId t1, S "~", typeToId t2]
typeToId (DataType t1 t2) = Comp [typeToId t1, S "*", typeToId t2]
typeToId (TypeName [t]) = typeToId t
typeToId (TypeName types) = Comp $ map typeToId types

idToType :: Id -> Type
idToType (S "int") = EnkiInt
idToType (S "string") = EnkiString
idToType (S "bool") = EnkiBool
idToType (S "void") = Void
idToType (S str) = Named str
idToType (V v) = Any v
idToType (Comp [t1, S "->", t2]) = FuncType (idToType t1) (idToType t2)
idToType (Comp [t1, S "~", t2]) = RuleType (idToType t1) (idToType t2)
idToType (Comp [t1, S "*", t2]) = DataType (idToType t1) (idToType t2)
idToType (Comp [id]) = idToType id
idToType (Comp ids) = TypeName $ map idToType ids
idToType (DefRef _ id) = idToType id

-- The first parameter is the id we wish to match (e.g., the function call expression),
-- the second is the function we are checking if it matches
unifyIds :: Id -> Id -> Maybe (Map String Id)
unifyIds (S s1) (S s2)       = if s1 == s2 then Just Map.empty else Nothing
unifyIds (I i1) (I i2)       = if i1 == i2 then Just Map.empty else Nothing
unifyIds id (V varName)      = Just $ Map.fromList [(varName, id)]
unifyIds (Comp []) (Comp []) = Just Map.empty
unifyIds id1 (Comp [id2])    = unifyIds id1 id2
unifyIds (Comp [id1]) id2    = unifyIds id1 id2
unifyIds (Comp (id1:ids1)) (Comp (id2:ids2)) = Map.union <$> unifyIds id1 id2 <*> unifyIds newId1 newId2
    where
        newId1 = unwrapIds ids1
        newId2 = unwrapIds ids2
unifyIds _ _ = Nothing

