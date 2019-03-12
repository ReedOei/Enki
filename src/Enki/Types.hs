module Enki.Types where

data Id = S String
        | I Integer
        | B Bool
        | V String
        | Comp [Id]
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

makeTypeName :: Id -> [Type]
makeTypeName (S str) = [Named str]
makeTypeName (V name) = [Any name]
makeTypeName (Comp []) = []
makeTypeName (Comp (id:ids)) = makeTypeName id ++ makeTypeName (Comp ids)

