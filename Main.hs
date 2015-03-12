module Main (main) where 

import Parser 
import Lexer 
import Compiler

import Control.Monad.State.Lazy
import Debug.Trace

-- TODO Associativity issue
testProg :: String
testProg = unlines 
  [ "aaa = (I (J (K 5) (K 6)))"
  , "main = (I (J (K 5) (K 6)))"
  , "I n = n"
  ]

main :: IO ()
main = putStrLn $ show out where
  out = do
    toks <- tokenize "" testProg
    tree <- parse toks
    trace ("Parse = " ++ (show (fst (head tree))))
      return ()
    Right $ evalState (rewrite (head tree)) LabelGen {count = 0}

    -- Right(tree)

    -- compileTLDef $ head ast 