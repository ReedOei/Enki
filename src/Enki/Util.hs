module Enki.Util where

indent :: Integral a => a -> String -> String
indent n = (concat (replicate (fromIntegral n) "    ") ++)

