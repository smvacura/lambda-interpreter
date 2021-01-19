module TypeCheck
    ( typeCheck
    ) where

import Parse (Expr(..))

data Type = TNat
          | TBool
          | TArr Type Type
          | Poly String
          | Failure
    deriving (Eq)


instance Show Type where
    show TNat = "Int"
    show TBool = "Bool"
    show (TArr t1 t2) = show t1 ++ " -> " ++ show t2


type AnnotatedAST = (Expr, Type)

type IDList = [String]

-- | Annotate types (even if incorrect)
typeAnnotate :: Expr -> IDList -> Type
typeAnnotate (ENum i) id = TNat
typeAnnotate (EBoolean b) id = TBool
typeAnnotate (Bound v) id = Poly "a"

-- | Check the types. Differs from 'typeAnnotate' in that
--   it rejects ill-formed expressions
typeCheck :: Expr -> Type
typeCheck (ENum i) = TNat
typeCheck (EBoolean b) = TBool
typeCheck (ArithBinop op e1 e2) = TArr TNat (TArr TNat TNat)
typeCheck (BoolBinop op e1 e2) = TArr TBool (TArr TBool TBool)
typeCheck (CompBinop op e1 e2) = TArr TNat (TArr TNat TBool)
typeCheck (If e1 e2 e3) = 
    if typeCheck e2 /= typeCheck e3 
    then Failure
    else case terminatorType $ typeCheck e1 of
            TBool -> TArr (typeCheck e1) (TArr (typeCheck e2) (typeCheck e3))
            _ -> Failure
typeCheck (Lambda v e) = typeCheck e


-- | Return the final type 
terminatorType :: Type -> Type
terminatorType TNat = TNat
terminatorType TBool = TBool
terminatorType (TArr t1 t2) = terminatorType t2