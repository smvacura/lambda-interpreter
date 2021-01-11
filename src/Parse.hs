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
litExpr = boolExpr <|> numExpr


lmbdaParse :: String -> Expr
lmbdaParse input = case parse (whitespace *> litExpr <* eof) "" input of
                     Left err -> EExn "error"
                     Right expr -> expr 