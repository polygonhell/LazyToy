module Parser (main) where

import Text.Parsec
-- import Data.Functor.Identity
import Text.ParserCombinators.Parsec.Pos
import Control.Applicative  ((<*))


import Lexer

type Parser a = Parsec [TokenPos] () a

data Prg
  = PrgApp Prg [Prg]
  | PrgId String
  | PrgFloat Double
  | PrgInt Integer
  | PrgTLDef String [String] Prg 
  deriving (Eq, Show)

type PrgPos = (Prg, Integer)

app :: Parser Prg
app = try $ do
  fn <- factor
  args <- many1 expr
  return $ PrgApp fn args

identifier :: Parser Prg
identifier = do
  TokenId name <- anyId
  return $ PrgId name

integer :: Parser Prg
integer = do
  TokenInt val <- anyInt
  return $ PrgInt val

expr :: Parser Prg
expr = 
      app
  <|> factor

factor :: Parser Prg
factor = 
      bracketed expr
  <|> identifier 
  <|> integer



unWrapId :: Token -> String
unWrapId (TokenId name) = name

topLevelDef :: Parser Prg
topLevelDef =  try $ do
  TokenId name <- anyId
  argTokens <- many anyId
  tok TokenEq
  e <- factor
  return $ PrgTLDef name (map unWrapId argTokens) e 


bracketed :: Parser a -> Parser a
bracketed = between (tok TokenLParen) (tok TokenRParen)


advance :: SourcePos -> t -> [TokenPos] -> SourcePos
advance _ _ ((_, pos) : _) = pos
advance pos _ [] = pos

-- Versions that retain position Info
satisfy2 :: (TokenPos -> Bool) -> Parser TokenPos
satisfy2 f = tokenPrim show
                      advance
                      (\c -> if f c then Just c else Nothing)

tok2 :: Token -> Parser TokenPos
tok2 t = (Parser.satisfy2 $ (== t) . fst) <?> show t

anyId2 :: Parser TokenPos
anyId2 = Parser.satisfy2 p <?> "Id"
  where p (t, _) = case t of 
                      TokenId _ -> True
                      _ -> False


satisfy :: (TokenPos -> Bool) -> Parser Token
satisfy f = tokenPrim show
                      advance
                      (\c -> if f c then Just (fst c) else Nothing)

tok :: Token -> Parser Token
tok t = (Parser.satisfy $ (== t) . fst) <?> show t

anyId :: Parser Token
anyId = Parser.satisfy p <?> "Id"
  where p (t, _) = case t of 
                      TokenId _ -> True
                      _ -> False

anyInt :: Parser Token
anyInt = Parser.satisfy p <?> "Int"
  where p (t, _) = case t of 
                      TokenInt _ -> True
                      _ -> False
                      

topLevel :: Parser [Prg]
topLevel = (many1 topLevelDef) <* (tok TokenEof)


parse ::SourceName -> [TokenPos] -> Either ParseError [Prg]
parse srcName tokenStream = runParser topLevel () srcName toks where 
  toks = tokenStream ++ [(TokenEof, initialPos "")]

testProg :: String
testProg = unlines 
  [ "main n = (fib n)"
  , "poop n = ((+ 3) n)"
  ]

-- TODO EOF Token
-- Deal with multiple SC defs

main :: IO ()
main = do
  putStrLn $ show $ tokenize "test" testProg
  -- tokenStream <- tokenize "test" "Hello There Now"
  putStrLn $ show $ Parser.parse "test" $ tokenStream 
    where tokenStream = case (tokenize "test" testProg) of
                          Right toks -> toks
                          _ -> error ("lexer failed")
