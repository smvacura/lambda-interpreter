{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Data.String       (fromString)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Parse (lmbdaParse, Expr(..), Oper(..))
import Eval (eval, saturate)


main :: IO ()
main = do 
    hspecWith defaultConfig {configFastFail = False} parseSpecs
    hspecWith defaultConfig {configFastFail = False} satSpecs
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
          expected=EExn "(line 1, column 1):\nunexpected end of input\nexpecting \"\\\\\", \"(\", \"if\", digit, \"true\", \"false\" or identifier"},
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
          expected=BoolBinop And (CompBinop Eq (ArithBinop Add (ENum 1) (ENum 2)) (ENum 4)) (CompBinop Parse.LT (ENum 1) (ArithBinop Add (ENum 3) (ENum 1)))},
          ExprCase {description="simple if-then-else",
          input="if 1 then 2 else 3",
          expected=If (ENum 1) (ENum 2) (ENum 3)},
          ExprCase {description="if-then-else with complex guard",
          input="if true or false then 1 else 2",
          expected=If (BoolBinop Or (EBoolean True) (EBoolean False)) (ENum 1) (ENum 2)},
          ExprCase {description="if-then-else if",
          input="if true then 1 else if false or true then 2 else 3",
          expected=If (EBoolean True) (ENum 1) (If (BoolBinop Or (EBoolean False) (EBoolean True)) (ENum 2) (ENum 3))},
          ExprCase {description="simple lambda",
          input="\\x.1",
          expected=Lambda "x" (ENum 1)},
          ExprCase {description="lambda with expr",
          input="\\x.x+1",
          expected=Lambda "x" (ArithBinop Add (Bound "x") (ENum 1))},
          ExprCase {description="double lambda",
          input="\\x.\\y.x + y",
          expected=Lambda "x" (Lambda "y" (ArithBinop Add (Bound "x") (Bound "y")))},
          ExprCase {description="lambda application single",
          input="\\x.x + 1 2",
          expected=App (Lambda "x" (ArithBinop Add (Bound "x") (ENum 1))) (ENum 2)},
          ExprCase {description="double application",
          input="\\x.\\y.x + y 1 2",
          expected=App (Lambda "x" (App (Lambda "y" (ArithBinop Add (Bound "x") (Bound "y"))) (ENum 1))) (ENum 2)}
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
    expected=EExn "TypeError: at operator `or` expected Bool but got Int"},
    ExprCase {description="simple if-then-else eval",
    input="if true then 1 else 2",
    expected=ENum 1},
    ExprCase {description="if-then-else complex guard eval",
    input="if true and false then 3 else 4",
    expected=ENum 4},
    ExprCase {description="if-then-else complex e1 e2",
    input="if true or false then 1 + 4 * 5 else 1 - 2 + 8",
    expected=ENum 21},
    ExprCase {description="simple fully-saturated lambda",
    input="\\x.x+1 1",
    expected=ENum 2},
    ExprCase {description="simple not-saturated lambda",
    input="\\x.\\y.x+y 1",
    expected=Lambda "x" (ArithBinop Add (Bound "x") (ENum 1))},
    ExprCase {description="double curried full-saturated lambda",
    input="\\x.\\y.x+y 1 2",
    expected=ENum 3}
    ]


satSpecs :: Spec
satSpecs = describe "Eval.saturate" $ for_ satCases test
    where

        test SatCase{..} = it satDescription assertion
            where
                assertion = saturate inputVar inputSub inputExpr `shouldBe` satExpected

data SatCase = SatCase {
                satDescription :: String,
                inputVar :: String,
                inputSub :: Expr,
                inputExpr :: Expr,
                satExpected :: Expr
}

satCases :: [SatCase]
satCases = [
    SatCase { satDescription="int literal no substitution",
    inputVar="x",
    inputSub=EBoolean True,
    inputExpr=ENum 2,
    satExpected=ENum 2},
    SatCase { satDescription="bool literal no substitution",
    inputVar="x",
    inputSub=ENum 1,
    inputExpr=EBoolean True,
    satExpected=EBoolean True},
    SatCase { satDescription="bound variable substitution",
    inputVar="x",
    inputSub=ENum 3,
    inputExpr=Bound "x",
    satExpected=ENum 3},
    SatCase {satDescription="bound variable NO substitution",
    inputVar="x",
    inputSub=ENum 4,
    inputExpr=Bound "z",
    satExpected=Bound "z"},
    SatCase {satDescription="bound variable embedded in binop",
    inputVar="y",
    inputSub=ENum 5,
    inputExpr=ArithBinop Add (Bound "y") (ENum 6),
    satExpected=ArithBinop Add (ENum 5) (ENum 6)}
    ]