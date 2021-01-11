module Main where

import System.IO ( hFlush, stdout )
import Parse as P
import Eval as E

main :: IO ()
main = do
    x <- prompt "Enter a string to parse: "
    let e = E.eval $ P.lmbdaParse x
    putStr e

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
