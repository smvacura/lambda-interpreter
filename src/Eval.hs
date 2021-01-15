module Eval 
    (eval,
     saturate,
     toString) where

import Parse
import qualified Data.Map as Map


-- | 'toString' converts an 'Expr' into its 'String' representation
toString :: Expr -> String
toString (ENum n) = show n ++ " :: Int"
toString (EBoolean b) = show b ++ " :: Bool"
toString (EExn exn) = exn
toString (Lambda v e) = show $ Lambda v e
toString expr = show expr


-- | 'toStringOper' converts an 'Oper' into its 'String' representation
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


-- | 'eval' completely evaluates an 'Expr' down to a literal form or lambda
--   ('ENum', 'EBoolean', or 'Lambda')
eval :: Expr -> Expr
eval e@(ENum n) = e
eval e@(EBoolean b) = e
eval e@(EExn exn) = e
eval (ArithBinop op e1 e2) = evalArithBinop op e1 e2
eval (BoolBinop op e1 e2) = evalBoolBinop op e1 e2
eval (CompBinop op e1 e2) = evalCompBinop op e1 e2
eval (If e1 e2 e3) = evalIf e1 e2 e3
eval (Lambda v (App e1 e2)) = Lambda v (evalApp e1 e2)
eval (Lambda v e) = Lambda v e
eval (App e1 e2) = evalApp e1 e2
eval expr = expr


-- | 'evalArithBinop' evaluates an arithmentic binary operation with operands
--   @e1@ and @e2@
--  
--   __Requires:__
--   * @op@ is a valid arithmetic operation
--   * @e1@ and @e2@ evaluate to @ENum@ literals
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


-- | 'evalBoolBinop' evaluates an boolean binary operation with operands
--   @e1@ and @e2@
--  
--   __Requires:__
--   * @op@ is a valid boolean operation
--   * @e1@ and @e2@ evaluate to @EBoolean@ literals
evalBoolBinop :: Oper -> Expr -> Expr -> Expr
evalBoolBinop op (EExn exn) e2 = EExn exn
evalBoolBinop op e1 (EExn exn) = EExn exn
evalBoolBinop op (ENum b) e2 = EExn $ constructTypeError op "Bool" "Int"
evalBoolBinop op e1 (ENum b) = EExn $ constructTypeError op "Bool" "Int"
evalBoolBinop And (EBoolean b1) (EBoolean b2) = EBoolean $ b1 && b2
evalBoolBinop Or (EBoolean b1) (EBoolean b2) = EBoolean $ b1 || b2
evalBoolBinop op e1 e2 = evalBoolBinop op (eval e1) (eval e2)


-- | 'evalCompBinop' evaluates an comparison binary operation with operands
--   @e1@ and @e2@
--  
--   __Requires:__
--   * @op@ is a valid comparison operation
--   * @e1@ and @e2@ evaluate to @ENum@ literals
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


-- | 'evalIf' evaluates an @if-then-else@ clause with guard @e1@
--   and branch expressions @e2@ and @e3@
--  
--   __Requires:__
--   * @e1@ evaluates to an @EBoolean@ literal
--   * @e2@ and @e3@ evaluate to literals with the same type
evalIf :: Expr -> Expr -> Expr -> Expr
evalIf (EBoolean b) e2 e3 = if b then eval e2 else eval e3
evalIf (ENum i) e2 e3 = EExn "TypeError: at `if` guard expected Bool but got Int"
evalIf e1 e2 e3 = evalIf (eval e1) e2 e3 


-- | 'evalApp' evaluates a lambda application with function expression 
--   @e1@ and argument expression @e2@
--  
--   __Requires:__
--   * @e1@ evaluates to a @Lambda@
--   * @e2@ evaluates to a literal
evalApp :: Expr -> Expr -> Expr
evalApp (Lambda v (App e1 e2)) e3 = evalApp (evalApp e1 e2) e3
evalApp lam@(Lambda v e1) e2 = 
    let saturatedLambda = saturate v e2 lam in
        case saturatedLambda of
            Lambda v e3 -> if isSaturated e3 
                           then eval e3 
                           else saturatedLambda
            _ -> EExn "Fatal error: impossible condition"
evalApp e1 e2 = EExn "Error: Tried to apply to a non-function"




constructTypeError :: Oper -> String -> String -> String 
constructTypeError op t1 t2 = "TypeError: at operator " ++ "`" ++ toStringOper op ++ "`" ++ " expected " ++ t1 ++ " but got " ++ t2


-- | 'saturate' performs one "saturation" of a lambda expression
-- 
--  @saturate v sube expr@ is @expr@ with all instances of @Bound v@
--  replaced by @sube@ (subsitute expr)
saturate :: String -> Expr -> Expr -> Expr
saturate v sube (App e1 e2) = 
    App (saturate v sube e1) (saturate v sube e2)
saturate v sube (Lambda v2 e) = 
    Lambda v2 (saturate v sube e)
saturate v sube (If e1 e2 e3) = 
    If (saturate v sube e1) (saturate v sube e2) (saturate v sube e3)
saturate v sube (ArithBinop op e1 e2) = 
    ArithBinop op (saturate v sube e1) (saturate v sube e2)
saturate v sube (BoolBinop op e1 e2) = 
    BoolBinop op (saturate v sube e1) (saturate v sube e2)
saturate v sube (CompBinop op e1 e2) = 
    CompBinop op (saturate v sube e1) (saturate v sube e2)
saturate v sube (ENum i) = ENum i
saturate v sube (EBoolean b) = EBoolean b
saturate v sube (EExn exn) = EExn exn
saturate v sube (Bound v2) = if v == v2 then sube else Bound v2


-- | 'isSaturated' is 'True' if there are no unsaturated bound
--   variables in 'expr' else 'False'
isSaturated :: Expr -> Bool
isSaturated (App e1 e2) = isSaturated e1 && isSaturated e2
isSaturated (Lambda v e) = isSaturated e
isSaturated (If e1 e2 e3) = isSaturated e1 && isSaturated e2 && isSaturated e3
isSaturated (ArithBinop op e1 e2) = isSaturated e1 && isSaturated e2
isSaturated (BoolBinop op e1 e2) = isSaturated e1 && isSaturated e2
isSaturated (CompBinop op e1 e2) = isSaturated e1 && isSaturated e2
isSaturated (ENum i) = True
isSaturated (EBoolean b) = True
isSaturated (Bound v) = False