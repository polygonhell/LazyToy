module Compiler (main) where

import Parser 
import Lexer 

compile :: [PrgPos] -> String
compile a = "Hello"







testProg :: String
testProg = unlines 
  [ "main n = (fib n)"
  , "poop n = ((+ 3) n)"
  ]

main :: IO ()
main = putStrLn $ show out where
  out = do
    toks <- tokenize "" testProg
    parse toks


