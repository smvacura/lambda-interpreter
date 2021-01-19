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
import Data.Functor.Identity


data Expr = ENum Integer 
          | EBoolean Bool
          | EExn String
          | ArithBinop Oper Expr Expr
          | BoolBinop Oper Expr Expr
          | CompBinop Oper Expr Expr
          | Uuop Oper Expr
          | If Expr Expr Expr
          | Lambda String Expr
          | Bound String
          | App Expr Expr
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
           | Eq 
           | NEq
           | GT
           | LT
           | GET
           | LET
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
litExpr =  try appExpr
       <|> try (lparens appExpr)
       <|> try (lparens lambdaExpr)
       <|> try lambdaExpr
       <|> lparens ifExpr
       <|> ifExpr
       <|> arithOpExpr 
       <|> boolExpr 
       <|> numExpr


lmbdaParse :: String -> Expr
lmbdaParse input = case parse (whitespace *> litExpr <* eof) "" input of
                     Left err -> EExn $ show err
                     Right expr -> expr 

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef
    where languageDef =
            emptyDef {  Token.identStart      = letter,
                        Token.identLetter     = letter,
                        Token.reservedNames   = [ "if",
                                                    "then",
                                                    "true",
                                                    "false",
                                                    "not",
                                                    "and",
                                                    "or",
                                                    "\\",
                                                    "."
                                                ],
                        Token.reservedOpNames = ["+", "-", "*", "/", "=",
                                                "<", ">", "and", "or", "not",
                                                "&"
                                                ]
            }


lidentifier :: ParsecT String () Identity String
lidentifier = Token.identifier lexer

lparens :: ParsecT String () Identity a -> ParsecT String () Identity a
lparens = Token.parens lexer

lreserved :: String -> Parser ()
lreserved = Token.reserved lexer

lreservedOp :: String -> Parser ()
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

lambdaExpr :: Parser Expr
lambdaExpr = do
    char '\\'
    bound <- lidentifier
    char '.'
    e <- litExpr
    return $ Lambda bound e


appExpr :: Parser Expr
appExpr = do
    e1 <- lparens lambdaExpr <|> lambdaExpr
    e2 <- lparens litExpr <|> litExpr
    return $ App e1 e2

arithOpExpr :: Parser Expr
arithOpExpr = buildExpressionParser arithOps arithTerm

arithTerm :: Parser Expr
arithTerm =  lparens litExpr
         <|> numExpr
         <|> boolExpr
         <|> fmap Bound lidentifier

arithOps = [[Infix  (lreservedOp "*" >> return (ArithBinop Mult)) AssocLeft,
             Infix  (lreservedOp "/" >> return (ArithBinop Div )) AssocLeft],
            [Infix (lreservedOp "+" >> return (ArithBinop Add)) AssocLeft,
             Infix (lreservedOp "-" >> return (ArithBinop Sub)) AssocLeft ],
            [Infix (lreservedOp "=" >> return (CompBinop Eq)) AssocLeft ,
             Infix (lreservedOp "<" >> return (CompBinop Parse.LT)) AssocLeft,
             Infix (lreservedOp ">" >> return (CompBinop Parse.GT)) AssocLeft ],
            [Infix (lreservedOp "and" >> return (BoolBinop And)) AssocLeft,
             Infix (lreservedOp "or" >> return (BoolBinop Or)) AssocLeft ]]
            