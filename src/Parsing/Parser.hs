{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parsing.Parser (parseProgram, ParsingError (..)) where

import Control.Applicative ((<|>), many)
import Control.Monad (void)
import Data.Functor (($>))
import Language.Name (mkName, discard, Name)
import Language.Core.Syntax (Term (..), Statement (..), Program)
import Parsing.Lexer (Token (..))
import Parsing.ParserM (FromStream, ParserM, fromStream, satisfies, pluck, runParser)
import Prelude hiding (pi)

data ParsingError
  = UnexpectedEOF
  | UnexpectedToken Token

type Parser a = ParserM [Token] ParsingError a

instance FromStream [Token] ParsingError where
  fromStream [] = UnexpectedEOF
  fromStream (t : _) = UnexpectedToken t

is :: Token -> Parser ()
is = void . satisfies . (==)

name :: Parser String
name = pluck $ \case
  (TName s) -> Just s
  _ -> Nothing

parseProgram :: [Token] -> Either ParsingError Program
parseProgram t = case runParser program t of
  Left error -> Left error
  Right (program, []) -> Right program
  Right (_, rest) -> Left $ fromStream rest

program :: Parser Program
program = many statement

statement :: Parser Statement
statement = definition <|> display
  where
    definition = do
      is TDef
      id <- fmap mkName name
      is TColon
      typ <- expression
      is TEquals
      body <- expression
      pure $ Define id $ Annotation body typ

    display = fmap Display $ is TDisplay *> expression


mkDiscardableName :: String -> Name
mkDiscardableName "_" = discard
mkDiscardableName name = mkName name

expression :: Parser Term
expression =
  pi
    <|> lambda
    <|> variable
    <|> application
    <|> annotation
    <|> universe
  where
    variable = do 
      id <- fmap mkDiscardableName name
      pure $ Variable id


    lambda = do
      is TLambda
      argName <- fmap mkName name
      is TDot
      body <- expression
      pure $ Lambda argName body

    pi = do
      is TPi
      is TOpenParen
      argName <- fmap mkName name
      is TColon
      typ <- expression
      is TCloseParen
      is TArrow
      body <- expression
      pure $ Pi argName typ body

    application = do
      is TOpenParen
      fun <- expression
      arg <- expression
      is TCloseParen
      pure $ Application fun arg

    annotation = do
      is TCloseParen
      term <- expression
      is TColon
      typ <- expression
      is TCloseParen
      pure $ Annotation term typ

    universe = is TUniverse $> Universe
