module Main (main) where


import CodeGen
import Compiler
import Lexer
import Parser
import Control.Monad.State.Lazy
import Text.Parsec.Error

import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid


justParse :: String -> Either ParseError [PrgPos]
justParse p =  do 
        toks <- tokenize "" p
        parse toks

main :: IO ()
main = defaultMainWithOpts
       [ testCase "rev" testRev
       ] mempty



compile :: String -> Either ParseError [PrgPos]
compile input = treeOut where
  rewrite i = evalState (factorArgs i) LabelGen {count = 0}
  treeOut = do
    toks <- tokenize "" input
    tree <- parse toks
    return $ map rewrite tree

testProg :: String
testProg = unlines
  [ "I x = x"
  , "main = (I (I (I 5)))"
  , "K x y = x"
  , "S x y z = (x z (y z))"
  , "Iprime x = (S K K x)"
  , "add a = (plus a)"
  , "test = (add 3 4)"
  ]

-- Just a failed test to run the code
testRev :: Assertion
testRev = do 
	(genCode $ compile testProg)
	False @? "Failed"


