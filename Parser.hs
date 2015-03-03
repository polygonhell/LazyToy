module Parser (main) where

import Text.Parsec
-- import Data.Functor.Identity
-- import Control.Applicative  ((<*), (*>), (<$>), (<*>))


import Lexer

type Parser a = Parsec [TokenPos] () a

data Prg
  = PrgApp Prg [Prg]
  | PrgId String
  | PrgFloat Double
  | PrgInt Integer
  deriving (Eq, Show)

app :: Parser Prg
app = try $ do
  fn <- factor
  args <- many1 expr
  return $ PrgApp fn args

identifier :: Parser Prg
identifier = do
  TokenId name <- anyId
  return $ PrgId name

expr :: Parser Prg
expr = 
      app
  <|> factor

factor :: Parser Prg
factor = 
      bracketed expr
  <|> identifier 
  


bracketed :: Parser a -> Parser a
bracketed = between (tok TokenLParen) (tok TokenRParen)


advance :: SourcePos -> t -> [TokenPos] -> SourcePos
advance _ _ ((_, pos) : _) = pos
advance pos _ [] = pos

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


parse ::SourceName -> [TokenPos] -> Either ParseError Prg
parse = runParser app ()

main :: IO ()
main = do
  putStrLn $ show $ tokenize "test" "Hello There (Now then)"
  -- tokenStream <- tokenize "test" "Hello There Now"
  putStrLn $ show $ Parser.parse "test" $ tokenStream 
    where tokenStream = case (tokenize "test" "Poop (Hello There)") of
                          Right toks -> toks
                          _ -> error ("lexer failed")
