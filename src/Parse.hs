module Parse
    ( Expr (..),
      lmbdaParse
    ) where

import Control.Applicative((<*))
import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language
import Data.Char
import Control.Monad

data Expr = ENum Integer 
          | EBoolean Bool
          | EExn String
          | Binop Oper Expr Expr
          | If Expr Expr Expr
          | Lmbda String Expr
    deriving (Eq, Show)

data Oper = Add
           | Sub
           | Mult
           | Div
           | Exp
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


addOp :: Parser Oper
addOp = do
    opChar <- char '+'
    return Add

subOp :: Parser Oper
subOp = do
    opChar <- char '-'
    return Sub

multOp :: Parser Oper
multOp = do
    opChar <- char '*'
    return Mult

divOp :: Parser Oper
divOp = do
    opChar <- char '/'
    return Div
    
binOp :: Parser Oper
binOp = addOp <|> subOp <|> multOp <|> divOp

binopExpr :: Parser Expr
binopExpr = do
    e1 <- litExpr
    op <- binOp
    e2 <- litExpr
    return $ Binop op e1 e2

litExpr :: Parser Expr
litExpr = boolExpr <|> numExpr

lmbdaParse :: String -> Expr
lmbdaParse input = case parse (whitespace *> litExpr <* eof) "" input of
                     Left err -> EExn $ show err
                     Right expr -> expr 