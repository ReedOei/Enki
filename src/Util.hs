module Util where

import Data.Char

toLowerCase :: String -> String
toLowerCase = map toLower

titleCaseWord :: String -> String
titleCaseWord [] = []
titleCaseWord (x:xs) = toUpper x : xs

titleCase :: String -> String
titleCase = unwords . map titleCaseWord . words

