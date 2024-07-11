{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Parsing.ParserM (ParserM, runParser, FromStream (..), satisfies, pluck, with) where

import Control.Applicative (Alternative (..))
import Control.Arrow (first)

newtype ParserM s e p = Parser
  { runParser :: s -> Either e (p, s)
  }

instance Functor (ParserM s e) where
  fmap f = Parser . fmap (fmap $ first f) . runParser

instance Applicative (ParserM s e) where
  pure p = Parser $ \s -> Right (p, s)

  (Parser atob) <*> (Parser a) =
    Parser $ \s -> do
      (atob, s) <- atob s
      (a, s) <- a s
      pure (atob a, s)

class FromStream s e where
  fromStream :: s -> e

instance (FromStream s e) => Alternative (ParserM s e) where
  empty = Parser $ Left . fromStream

  (Parser a) <|> (Parser b) =
    Parser $ \s -> case (a s, b s) of
      (Left _, b) -> b
      (a, _) -> a

satisfies :: (FromStream [c] e) => (c -> Bool) -> ParserM [c] e c
satisfies p = Parser $ \case
  c : cs | p c -> Right (c, cs)
  cs -> Left $ fromStream cs

pluck :: (FromStream [c] e) => (c -> Maybe a) -> ParserM [c] e a
pluck f =
  Parser $ \case
    t : ts -> case f t of
      Just res -> Right (res, ts)
      Nothing -> Left $ fromStream $ t : ts
    ts -> Left $ fromStream ts

with :: (Functor f) => b -> f a -> f b
with a = fmap (const a)
