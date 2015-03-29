module Main (main) where

import           CodeGen
import           Compiler
import           Lexer
import           Parser

import           Control.Monad.State.Lazy
import           Text.Parsec.Error
-- import Debug.Trace

-- TODO Associativity issue
testProg :: String
testProg = unlines
  [ "I x = x"
  , "main = (I 245)"
  -- , "m = (I (I (I 5)))"
  -- , "K x y = x"
  -- , "S x y z = (x z (y z))"
  -- , "Iprime x = (S K K x)"
  -- , "add a = (plus a)"
  -- , "test = (add 3 4)"
  ]


compile :: String -> Either ParseError [PrgPos]
compile input = treeOut where
  rewrite i = evalState (factorArgs i) LabelGen {count = 0}
  treeOut = do
    toks <- tokenize "" input
    tree <- parse toks
    return $ map rewrite tree



main :: IO ()
main = do
  putStrLn $ show $ compile testProg
  genCode $ compile testProg
