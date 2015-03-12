module Main (main) where

import Lexer
import Parser
import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Text.Parsec hiding (parse)
import Data.Monoid

justParse :: String -> Either ParseError [PrgPos]
justParse p =  do 
        toks <- tokenize "" p
        parse toks

main :: IO ()
main = defaultMainWithOpts
       [ testCase "rev" testRev
       ] mempty

testRev :: Assertion
testRev = reverse [1, 2, 3] @?= [3, 3, 1]
