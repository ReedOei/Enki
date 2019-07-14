module Enki.Types where

import Data.List

data Id = S String
        | I Integer
        | B Bool
        | V String
        | Comp [Id]
        | DefRef Integer Id -- e.g., (pair of _ and _) = DefRef 2 (Comp [S "pair", S "of", V "_1", S "and", V "_2"])
    deriving (Eq, Show)

data Type = EnkiInt
          | EnkiBool
          | EnkiString
          | Any String
          | Void
          | FuncType Type Type
          | RuleType Type Type
          | DataType Type Type
          | Named String
          | TypeName [Type]
    deriving (Eq, Show)

makeTypeName :: Id -> Type
makeTypeName (S str) = Named str
makeTypeName (V name) = Any name
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

