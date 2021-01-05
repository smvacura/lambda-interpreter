module Main where

import System.IO ( hFlush, stdout )

main :: IO ()
main = do
    x <- prompt "Enter a string to echo: "
    putStr x

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
