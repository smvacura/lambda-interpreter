module Eval 
    (eval,
     toString) where

import Parse
import qualified Data.Map as Map

toString :: Expr -> String
toString (ENum n) = show n ++ " :: Int"
toString (EBoolean b) = show b ++ " :: Bool"
toString (EExn exn) = exn
toString (Lambda v e) = "λ :: (type inference unimplemented)"
toString _ = "Failure: unimplemented"

toStringOper :: Oper -> String
toStringOper Add = "+"
toStringOper Sub = "-"
toStringOper Mult = "*"
toStringOper Div = "/"
toStringOper And = "and"
toStringOper Or = "or"
toStringOper Eq = "="
toStringOper Parse.LT = "<"
toStringOper Parse.GT = ">"
toStringOper Parse.LET = "<="
toStringOper Parse.GET = ">="

eval :: Expr -> Expr
eval e@(ENum n) = e
eval e@(EBoolean b) = e
eval e@(EExn exn) = e
eval (ArithBinop op e1 e2) = evalArithBinop op e1 e2
eval (BoolBinop op e1 e2) = evalBoolBinop op e1 e2
eval (CompBinop op e1 e2) = evalCompBinop op e1 e2
eval (If e1 e2 e3) = evalIf e1 e2 e3
eval (Lambda v e) = Lambda v e
eval _ = EExn "unimplemented"

evalArithBinop :: Oper -> Expr -> Expr -> Expr
evalArithBinop op (EExn exn) e2 = EExn exn
evalArithBinop op e1 (EExn exn) = EExn exn
evalArithBinop op (EBoolean b) e2 = EExn $ constructTypeError op "Int" "Bool"
evalArithBinop op e1 (EBoolean b) = EExn $ constructTypeError op "Int" "Bool"
evalArithBinop Add (ENum i1) (ENum i2) = ENum $ i1 + i2
evalArithBinop Sub (ENum i1) (ENum i2) = ENum $ i1 - i2
evalArithBinop Mult (ENum i1) (ENum i2) = ENum $ i1 * i2
evalArithBinop Div (ENum i1) (ENum i2) = ENum $ i1 `div` i2
evalArithBinop op e1 e2 = evalArithBinop op (eval e1) (eval e2)


evalBoolBinop :: Oper -> Expr -> Expr -> Expr
evalBoolBinop op (EExn exn) e2 = EExn exn
evalBoolBinop op e1 (EExn exn) = EExn exn
evalBoolBinop op (ENum b) e2 = EExn $ constructTypeError op "Bool" "Int"
evalBoolBinop op e1 (ENum b) = EExn $ constructTypeError op "Bool" "Int"
evalBoolBinop And (EBoolean b1) (EBoolean b2) = EBoolean $ b1 && b2
evalBoolBinop Or (EBoolean b1) (EBoolean b2) = EBoolean $ b1 || b2
evalBoolBinop op e1 e2 = evalBoolBinop op (eval e1) (eval e2)


evalCompBinop :: Oper -> Expr -> Expr -> Expr
evalCompBinop op (EExn exn) e2 = EExn exn
evalCompBinop op (EBoolean b) e2 = EExn $ constructTypeError op "Int" "Bool"
evalCompBinop op e1 (EBoolean b) = EExn $ constructTypeError op "Int" "Bool"
evalCompBinop Eq (ENum i1) (ENum i2) = EBoolean $ i1 == i2
evalCompBinop Parse.LT (ENum i1) (ENum i2) = EBoolean $ i1 < i2
evalCompBinop Parse.GT (ENum i1) (ENum i2) = EBoolean $ i1 > i2
evalCompBinop Parse.LET (ENum i1) (ENum i2) = EBoolean $ i1 <= i2
evalCompBinop Parse.GET (ENum i1) (ENum i2) = EBoolean $ i1 >= i2
evalCompBinop op e1 e2 = evalCompBinop op (eval e1) (eval e2)


evalIf :: Expr -> Expr -> Expr -> Expr
evalIf (EBoolean b) e2 e3 = if b then eval e2 else eval e3
evalIf (ENum i) e2 e3 = EExn "TypeError: at `if` guard expected Bool but got Int"
evalIf e1 e2 e3 = evalIf (eval e1) e2 e3 


constructTypeError :: Oper -> String -> String -> String 
constructTypeError op t1 t2 = "TypeError: at operator " ++ "`" ++ toStringOper op ++ "`" ++ " expected " ++ t1 ++ " but got " ++ t2