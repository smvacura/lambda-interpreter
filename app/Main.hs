module Main where

import System.IO ( hFlush, stdout )
import GHC.IO.Encoding ( utf8, setLocaleEncoding )
import qualified Parse as P
import qualified Eval as E

main :: IO ()
main = do
    setLocaleEncoding utf8
    x <- prompt "Enter a string to parse: "
    let e = E.toString $ E.eval $ P.lmbdaParse x
    putStr e

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
