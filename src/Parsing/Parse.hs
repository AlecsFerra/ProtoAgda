module Parsing.Parse (parse, ParseError) where

import qualified Parsing.Lexer as L
import qualified Parsing.Parser as P
import Data.Bifunctor (first)
import Language.Term (Program)

data ParseError
  = UnexpectedEOF
  | UnexpectedToken L.Token
  | UnexpectedChar Char
  deriving (Show)

parse :: String -> Either ParseError Program
parse src = do
  lexed <- first lexToParseError $ L.lexProgram src
  first parseToParseError $ P.parseProgram lexed
  


lexToParseError :: L.LexingError -> ParseError
lexToParseError L.UnexpectedEOF = UnexpectedEOF
lexToParseError (L.UnexpectedChar chr) = UnexpectedChar chr

parseToParseError :: P.ParsingError -> ParseError
parseToParseError P.UnexpectedEOF = UnexpectedEOF
parseToParseError (P.UnexpectedToken tok) = UnexpectedToken tok 

