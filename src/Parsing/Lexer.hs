{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parsing.Lexer (Token (..), lexProgram, LexingError (..)) where

import Control.Applicative (many, some, (<|>))
import Control.Monad (void)
import Data.Char (isAlpha)
import Parsing.ParserM (FromStream, ParserM, fromStream, runParser, satisfies, with)
import Prelude hiding (lex)

data Token
  = TColon
  | TLambda
  | TPi
  | TUniverse
  | TName String
  | TOpenParen
  | TCloseParen
  | TDef
  | TEquals
  | TDisplay
  | TArrow
  | TDot
  deriving (Eq)

instance Show Token where
  show TColon = ":"
  show TLambda = "λ"
  show TPi = "Π"
  show TUniverse = "𝒰"
  show (TName name) = name
  show TOpenParen = "("
  show TCloseParen = ")"
  show TDef = "def"
  show TEquals = "="
  show TDisplay = "display"
  show TArrow = "→"
  show TDot = "."

data LexingError
  = UnexpectedEOF
  | UnexpectedChar Char

type Lexer = ParserM String LexingError

instance FromStream String LexingError where
  fromStream [] = UnexpectedEOF
  fromStream (c : _) = UnexpectedChar c

char :: Char -> Lexer Char
char = satisfies . (==)

string :: String -> Lexer String
string = traverse char

whitespace :: Lexer ()
whitespace = void (char ' ' <|> char '\t' <|> char '\n')

keywords :: Lexer Token
keywords =
  TLambda `with` string "lambda"
    <|> TPi `with` string "Pi"
    <|> TUniverse `with` string "U"
    <|> TDef `with` string "def"
    <|> TDisplay `with` string "display"

symbols :: Lexer Token
symbols =
  TColon `with` string ":"
    <|> TLambda `with` string "λ"
    <|> TPi `with` string "Π"
    <|> TUniverse `with` string "𝒰"
    <|> TOpenParen `with` string "("
    <|> TCloseParen `with` string ")"
    <|> TEquals `with` string "="
    <|> TArrow `with` string "->"
    <|> TArrow `with` string "→"
    <|> TDot `with` string "."

name :: Lexer Token
name = TName <$> some (satisfies isAlpha <|> satisfies (== '_'))

token :: Lexer Token
token = keywords <|> symbols <|> name

lex :: Lexer [Token]
lex = many (many whitespace *> token) <* many whitespace

lexProgram :: String -> Either LexingError [Token]
lexProgram s = case runParser lex s of
  Left error -> Left error
  Right (tokens, "") -> Right tokens
  Right (_, rest) -> Left $ fromStream rest
