module Main (main) where 

import Parser 
import Lexer 
import Compiler

import Control.Monad.State.Lazy
-- import Debug.Trace


testProg :: String
testProg = unlines 
  [ "main = (I (J (K 5)))"
  , "I n = n"
  ]

main :: IO ()
main = putStrLn $ show out where
  out = do
    toks <- tokenize "" testProg
    tree <- parse toks
    Right $ evalState (rewrite (head tree)) LabelGen {count = 0}

    -- Right(tree)

    -- compileTLDef $ head ast 