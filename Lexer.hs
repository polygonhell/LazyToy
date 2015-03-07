module Lexer (tokenize, Token(..), TokenPos) where 

import Text.Parsec hiding (token, tokens)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as Tok
-- import Text.Parsec.Indent

import Control.Applicative  ((<*), (*>), (<$>), (<*>))


data Token 
  = TokenId String
  | TokenFloat Double
  | TokenInt Integer
  | TokenString String
  | TokenLParen
  | TokenRParen
  | TokenEq
  | TokenEof
  deriving (Eq,Show)

type TokenPos = (Token, SourcePos)


langDef :: Tok.LanguageDef ()
langDef = Tok.LanguageDef
  { Tok.commentStart    = "{-"
  , Tok.commentEnd      = "-}"
  , Tok.commentLine     = "--"
  , Tok.nestedComments  = True
  , Tok.identStart      = letter
  , Tok.identLetter     = alphaNum <|> oneOf "_'"
  , Tok.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~"
  , Tok.reservedNames   = []
  , Tok.reservedOpNames = []
  , Tok.caseSensitive   = True
  }

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser langDef

whiteSpace :: Parser ()
whiteSpace = Tok.whiteSpace lexer

operator :: Parser Token
operator = do 
  p <- Tok.operator lexer
  return $ TokenId p


identifier :: Parser Token
identifier = do 
  p <- Tok.identifier lexer
  return $ TokenId p

lparen, rparen, equals :: Parser Token
lparen = char '(' >> return TokenLParen
rparen = char ')' >> return TokenRParen
equals = char '=' >> return TokenEq

int :: Parser Token
int = do
  p <- Tok.integer lexer
  return $ TokenInt p

float :: Parser Token
float = do
  p <- Tok.float lexer
  return $ TokenFloat p

stringLit :: Parser Token
stringLit = do
  p <- Tok.stringLiteral lexer
  return $ TokenString p


-- Attach the positional information
parsePos :: Parser Token -> Parser TokenPos
parsePos p = flip (,) <$> getPosition <*> p

token :: Parser TokenPos
token = parsePos $ choice 
    [ lparen
    , rparen
    , equals
    , operator
    , identifier
    , int
    , float
    , stringLit
    ]


tokens :: Parser [TokenPos]
tokens = whiteSpace *> many (token <* whiteSpace) <* eof
 

tokenize :: SourceName -> String -> Either ParseError [TokenPos]
tokenize = runParser tokens ()



