{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Foldable     (for_)
import Data.String       (fromString)
import Test.Hspec        (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)

import Parse (lmbdaParse, Expr(..))


main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs


specs :: Spec
specs = describe "lmbdaParse" $ for_ cases test
    where

        test Case{..} = it description assertion
            where
                assertion = lmbdaParse input `shouldBe` expected

data Case = Case { description :: String
                 , input       :: String
                 , expected    :: Expr
                 }

cases :: [Case]
cases = [ Case {description="empty string",
          input="",
          expected=EExn "(line 1, column 1):\nunexpected end of input\nexpecting \"true\", \"false\" or digit"},
          Case {description="basic numeral",
          input="2",
          expected=ENum 2},
          Case {description="multi-digit numeral",
          input="123456789",
          expected=ENum 123456789},
          Case {description="bool true",
          input="true",
          expected=EBoolean True},
          Case {description="bool false",
          input="false",
          expected=EBoolean False}
        ]