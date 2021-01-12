module Eval 
    (eval,
     toString) where

import Parse

toString :: Expr -> String
toString (ENum n) = show n ++ " :: Int"
toString (EBoolean b) = show b ++ " :: Bool"
toString (EExn exn) = exn
toString _ = "Failure: unimplemented"

eval :: Expr -> Expr
eval e@(ENum n) = e
eval e@(EBoolean b) = e
eval e@(EExn exn) = e
eval (ArithBinop op e1 e2) = evalArithBinop op e1 e2
eval _ = EExn "unimplemented"

evalArithBinop :: Oper -> Expr -> Expr -> Expr
evalArithBinop op (EExn exn) e2 = EExn exn
evalArithBinop op e1 (EExn exn) = EExn exn
evalArithBinop op (EBoolean b) e2 = EExn "TypeError: Expected Int but got Bool"
evalArithBinop op e1 (EBoolean b) = EExn "TypeError: Expected Int but got Bool"
evalArithBinop Add (ENum i1) (ENum i2) = ENum $ i1 + i2
evalArithBinop Sub (ENum i1) (ENum i2) = ENum $ i1 - i2
evalArithBinop Mult (ENum i1) (ENum i2) = ENum $ i1 * i2
evalArithBinop Div (ENum i1) (ENum i2) = ENum $ i1 `div` i2
evalArithBinop op e1 e2 = evalArithBinop op (eval e1) (eval e2)


evalBoolBinop :: Oper -> Expr -> Expr -> Expr
evalBoolBinop op (EExn exn) e2 = EExn exn
evalBoolBinop op e1 (EExn exn) = EExn exn
evalBoolBinop op (ENum b) e2 = EExn "TypeError: Expected Bool but got Int"
evalBoolBinop op e1 (ENum b) = EExn "TypeError: Expected Bool but got Int"
evalBoolBinop And (EBoolean b1) (EBoolean b2) = EBoolean $ b1 && b2