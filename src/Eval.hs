module Eval 
    (eval) where

import Parse

eval :: Expr -> String
eval (ENum n) = show n ++ " :: Int"
eval (EBoolean b) = show b ++ " :: Bool"
eval (EExn exn) = exn
eval _ = "unimplemented"