module Parser (parse, Prg(..), PrgPos) where

import Text.Parsec hiding (parse)
-- import Data.Functor.Identity
import Text.ParserCombinators.Parsec.Pos
import Control.Applicative  ((<*), (*>))


import Lexer

type Parser a = Parsec [TokenPos] () a

data Prg
  = PrgApp PrgPos [PrgPos]
  | PrgId String
  | PrgFloat Double
  | PrgInt Integer
  | PrgTLDef String [String] PrgPos
  deriving (Eq, Show)

type PrgPos = (Prg, SourcePos)

app :: Parser PrgPos
app = try $ do
  (fn, pos) <- factor
  argsWithPos <- many1 expr
  return $ (PrgApp (fn,pos) argsWithPos, pos) 

identifier :: Parser PrgPos
identifier = do
  (TokenId name, pos) <- anyId
  return $ (PrgId name, pos)

integer :: Parser PrgPos
integer = do
  (TokenInt val, pos) <- anyInt
  return $ (PrgInt val, pos)

expr :: Parser PrgPos
expr = 
      app
  <|> factor

factor :: Parser PrgPos
factor = 
      bracketed expr
  <|> identifier 
  <|> integer



unWrapId :: TokenPos -> String
unWrapId ((TokenId name), _) = name

topLevelDef :: Parser PrgPos
topLevelDef =  try $ do
  (TokenId name, pos) <- anyId
  argTokens <- many anyId
  tok TokenEq
  e <- factor
  return $ (PrgTLDef name (map unWrapId argTokens) e, pos) 


bracketed :: Parser a -> Parser a
bracketed x = tok TokenLParen *> x <* tok TokenRParen

advance :: SourcePos -> t -> [TokenPos] -> SourcePos
advance _ _ ((_, pos) : _) = pos
advance pos _ [] = pos

satisfy :: (TokenPos -> Bool) -> Parser TokenPos
satisfy f = tokenPrim show
                      advance
                      (\c -> if f c then Just c else Nothing)

tok :: Token -> Parser TokenPos
tok t = (Parser.satisfy $ (== t) . fst) <?> show t

anyId :: Parser TokenPos
anyId = Parser.satisfy p <?> "Id"
  where p (t, _) = case t of 
                      TokenId _ -> True
                      _ -> False

anyInt :: Parser TokenPos
anyInt = Parser.satisfy p <?> "Int"
  where p (t, _) = case t of 
                      TokenInt _ -> True
                      _ -> False
                      

topLevel :: Parser [PrgPos]
topLevel = (many1 topLevelDef) <* (tok TokenEof)


parse :: [TokenPos] -> Either ParseError [PrgPos]
parse  tokenStream = runParser topLevel () "" toks where 
  toks = tokenStream ++ [(TokenEof, initialPos "")]

