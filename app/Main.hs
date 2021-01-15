module Main where

import System.IO ( hFlush, stdout )
import GHC.IO.Encoding ( utf8, setLocaleEncoding )
import qualified Parse as P
import qualified Eval as E
import Control.Monad ( unless )

main :: IO ()
main = do
    setLocaleEncoding utf8
    input <- readREPL
    unless (input == ":quit")
        $ printREPL (evalREPL input) >> main


-- | 'prompt' displays @text@ and returns @IO userinput@
prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine


-- | Carry out the "read" step of a REPL and return user input
readREPL :: IO String
readREPL = do
    putStr "Lambda>"
    hFlush stdout
    getLine


-- | Carry out the "eval" step of a REPL and return evaluated user input
evalREPL :: String -> String
evalREPL = E.toString . E.eval . P.lmbdaParse


-- | Carry out the "print" step of a REPL. 
--
--   Alias of 'putStr'
printREPL :: String -> IO ()
printREPL i = putStr i >> putStr "\n"