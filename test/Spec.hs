{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Data.String       (fromString)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Parse (lmbdaParse, Expr(..), Oper(..))
import Eval (eval)


main :: IO ()
main = do 
    hspecWith defaultConfig {configFastFail = False} parseSpecs
    hspecWith defaultConfig {configFastFail = False} evalSpecs


parseSpecs :: Spec
parseSpecs = describe "Parse.lmbdaParse" $ for_ parseCases test
    where

        test ExprCase{..} = it description assertion
            where
                assertion = lmbdaParse input `shouldBe` expected

data ExprCase = ExprCase { description :: String,
                 input       :: String,
                 expected    :: Expr
                 }

parseCases :: [ExprCase]
parseCases = [ ExprCase {description="empty string",
          input="",
          expected=EExn "(line 1, column 1):\nunexpected end of input\nexpecting digit, \"true\", \"false\" or identifier"},
          ExprCase {description="basic numeral",
          input="2",
          expected=ENum 2},
          ExprCase {description="multi-digit numeral",
          input="123456789",
          expected=ENum 123456789},
          ExprCase {description="bool true",
          input="true",
          expected=EBoolean True},
          ExprCase {description="bool false",
          input="false",
          expected=EBoolean False},
          ExprCase {description="simple binop +",
          input="1+2",
          expected=ArithBinop Add (ENum 1) (ENum 2)},
          ExprCase {description="simple binop -",
          input="1-2",
          expected=ArithBinop Sub (ENum 1) (ENum 2)},
          ExprCase {description="nested binop +",
          input="1+2+3",
          expected=ArithBinop Add (ArithBinop Add (ENum 1) (ENum 2)) (ENum 3)},
          ExprCase {description="arith with type error",
          input="1 + true",
          expected=ArithBinop Add (ENum 1) (EBoolean True)},
          ExprCase {description="bool binop",
          input="true and false",
          expected=BoolBinop And (EBoolean True) (EBoolean False)},
          ExprCase {description="or binop",
          input="true or false",
          expected=BoolBinop Or (EBoolean True) (EBoolean False)},
          ExprCase {description="mixing bool and arith binops",
          input="1 + true and false",
          expected=BoolBinop And (ArithBinop Add (ENum 1) (EBoolean True)) (EBoolean False)},
          ExprCase {description="arith and comp precedence",
          input="1 + 2 = 3",
          expected=CompBinop Eq (ArithBinop Add (ENum 1) (ENum 2)) (ENum 3)},
          ExprCase {description="all three binop precedence",
          input="1 + 2 = 4 and 1 < 3 + 1",
          expected=BoolBinop And (CompBinop Eq (ArithBinop Add (ENum 1) (ENum 2)) (ENum 4)) (CompBinop Parse.LT (ENum 1) (ArithBinop Add (ENum 3) (ENum 1)))}
        ] 


evalSpecs :: Spec
evalSpecs = describe "Eval.eval" $ for_ evalCases test
    where

        test ExprCase{..} = it description assertion
            where
                assertion = eval (lmbdaParse input) `shouldBe` expected

evalCases :: [ExprCase]
evalCases = [
    ExprCase {description="int literal",
    input="3",
    expected=ENum 3},
    ExprCase {description="bool literal",
    input="true",
    expected=EBoolean True},
    ExprCase {description="arith binop",
    input="1+2",
    expected=ENum 3},
    ExprCase {description="arith binop multiple",
    input="1+2+3+4+5",
    expected=ENum 15},
    ExprCase {description="arith binop mult",
    input="1*2",
    expected=ENum 2},
    ExprCase {description="arith test eval precedence",
    input="1+2*3",
    expected=ENum 7},
    ExprCase {description="arith binop type error",
    input="1+true",
    expected=EExn "TypeError: at operator `+` expected Int but got Bool"},
    ExprCase {description="bool binop",
    input="true and false",
    expected=EBoolean False},
    ExprCase {description="bool mix and or",
    input="true and false or true",
    expected=EBoolean True},
    ExprCase {description="bool binop type error",
    input="1 or false",
    expected=EExn "TypeError: at operator `or` expected Bool but got Int"}
    ]