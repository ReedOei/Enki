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
          | TypeName Id
    deriving (Eq, Show)

