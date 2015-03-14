module Main (main) where 

import Parser 
import Lexer 
import Compiler

import Text.Parsec.Error
import Control.Monad.State.Lazy
-- import Debug.Trace

-- TODO Associativity issue
testProg :: String
testProg = unlines 
  [ "main = (I (I (I 5)))"
  , "I x = x"
  , "K x y = x"
  , "S x y z = (x z (y z))"
  , "Iprime x = (S K K x)"
  , "add a = (plus a)"
  , "test = (add 3 4)"
  ]


compile :: String -> Either ParseError [PrgPos]
compile input = treeOut where 
  treeOut = do
    toks <- tokenize "" input
    tree <- parse toks
    return $ map rewrite tree 
  -- map rewrite tree where
  rewrite i = evalState (factorArgs i) LabelGen {count = 0}


k x y = x
s x y z = x z (y z)


main :: IO ()
main = do 
  putStrLn $ show $ compile testProg
  putStrLn $ show $ s k k 5
