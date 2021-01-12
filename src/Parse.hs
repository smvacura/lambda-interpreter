module Parse
    ( Expr (..),
      Oper(..),
      lmbdaParse
    ) where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Language
import Data.Char
import Control.Monad


data Expr = ENum Integer 
          | EBoolean Bool
          | EExn String
          | ArithBinop Oper Expr Expr
          | BoolBinop Oper Expr Expr
          | Uuop Oper Expr
          | If Expr Expr Expr
          | Lmbda String Expr
          | Bound String
    deriving (Eq, Show)

data Oper = Add
           | Sub
           | Mult
           | Div
           | Exp
           | Neg
           | And
           | Or
           | Not
    deriving (Eq, Show)

safeBoolRead :: String -> Bool
safeBoolRead "true" = True 
safeBoolRead _ = False

whitespace :: Parser ()
whitespace = skipMany $ oneOf " \n\t"

lexeme :: Parser a -> Parser a
lexeme p = do
           x <- p
           whitespace
           return x

numExpr :: Parser Expr
numExpr = do
    i <- (Parse.lexeme . many1) digit
    return $ ENum $ read i


boolExpr :: Parser Expr
boolExpr = do
    b <- Parse.lexeme $ string "true" <|> string "false"
    return $ EBoolean $ safeBoolRead b


litExpr :: Parser Expr
litExpr = arithExpr <|> boolExpr <|> numExpr


lmbdaParse :: String -> Expr
lmbdaParse input = case parse (whitespace *> litExpr <* eof) "" input of
                     Left err -> EExn $ show err
                     Right expr -> expr 



languageDef =
   emptyDef { Token.identStart      = letter,
              Token.identLetter     = letter,
              Token.reservedNames   = [ "if",
                                        "then",
                                        "true",
                                        "false",
                                        "not",
                                        "and",
                                        "or",
                                        "\\"
                                      ],
              Token.reservedOpNames = ["+", "-", "*", "/", "=",
                                       "<", ">", "and", "or", "not"
                                      ]
            }

lexer = Token.makeTokenParser languageDef

lidentifier = Token.identifier lexer

lreserved = Token.reserved lexer

lreservedOp = Token.reservedOp lexer

ifExpr :: Parser Expr
ifExpr = do
    lreserved "if"
    cond <- litExpr
    lreserved "then"
    e1 <- litExpr
    lreserved "else"
    e2 <- litExpr
    return $ If cond e1 e2

arithExpr :: Parser Expr
arithExpr = buildExpressionParser arithOps arithTerm

arithTerm :: Parser Expr
arithTerm = numExpr
         <|> boolExpr
         <|> fmap Bound lidentifier

arithOps = [  [Infix  (lreservedOp "*" >> return (ArithBinop Mult)) AssocLeft,
                Infix  (lreservedOp "/" >> return (ArithBinop Div )) AssocLeft],
              [Infix (lreservedOp "+" >> return (ArithBinop Add)) AssocLeft,
               Infix (lreservedOp "-" >> return (ArithBinop Sub)) AssocLeft ]]

bOperators = [ [Infix  (lreservedOp "and" >> return (BoolBinop And     )) AssocLeft,
                 Infix  (lreservedOp "or"  >> return (BoolBinop Or      )) AssocLeft]
              ]

            