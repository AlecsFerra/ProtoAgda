{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parsing.Parser (parseProgram, ParsingError (..)) where

import Control.Applicative (many, (<|>), some)
import Control.Monad (void)
import Language.Surface.Syntax (Program, Statement (..), Term (..), Name)
import Parsing.Lexer (Token (..))
import Parsing.ParserM (FromStream, ParserM, fromStream, pluck, runParser, satisfies)
import Prelude hiding (pi)
import Data.Foldable (foldl')

data ParsingError
  = UnexpectedEOF
  | UnexpectedToken Token

type Parser a = ParserM [Token] ParsingError a

instance FromStream [Token] ParsingError where
  fromStream [] = UnexpectedEOF
  fromStream (t : _) = UnexpectedToken t

is :: Token -> Parser ()
is = void . satisfies . (==)

name :: Parser Name
name = pluck $ \case
  (TName s) -> Just s
  _ -> Nothing

-- opsL :: Parser (a -> a -> a) -> Parser a -> Parser a
-- opsL psep pa = liftA2 squash pa (many (liftA2 (,) psep pa))
--     where
--         squash = foldl' (\acc (combine, a) -> combine acc a)

opsR :: Parser (a -> a -> a) -> Parser a -> Parser a
opsR psep pa = liftA2 squash pa (many (liftA2 (,) psep pa))
    where
        shift (oldStart, stack) (combine, a) =
            (a, (combine, oldStart) : stack)
        squash start annotated =
            let (start', annotated') = foldl' shift (start, []) annotated
             in foldl' (\acc (combine, a) -> combine a acc) start' annotated'

parensed :: Parser a -> Parser a
parensed p = is TOpenParen *> p <* is TCloseParen

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
      id <- name
      is TColon
      typ <- term
      is TEquals
      body <- term
      pure $ Define id typ body

    display = fmap Display $ is TDisplay *> term

term :: Parser Term
term = nonOperator <|> binaryOperator

nonOperator :: Parser Term
nonOperator = lambda <|> pi
  where
    lambda = do
      is TLambda
      argNames <- some name
      is TDot
      body <- term
      pure $ Lambda argNames body

    pi = do
      is TPi
      is TOpenParen
      argNames <- some name
      is TColon
      typ <- term
      is TCloseParen
      is TArrow
      body <- term        
      pure $ Pi argNames typ body

-- From the lowest to the highest precedence
binaryOperator :: Parser Term
binaryOperator = arrow
  where
    arrow = opsR (Arrow <$ is TArrow) annotation
    annotation = opsR (Annotation <$ is TColon) nullaryOperator

nullaryOperator :: Parser Term
nullaryOperator = application
  where
    application = build <$> some mono 

    build [] = error "Unreachable!! emptylist on some"
    build (x:xs) = foldl' Application x xs

mono :: Parser Term
mono = variable <|> universe <|> parensed term
  where
    variable = Variable <$> name
    universe = is TUniverse *> pure Universe
