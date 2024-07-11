module Language.Environment (Environment, empty, lookup, extend, on) where

import Data.Foldable (find)
import Prelude hiding (lookup)

newtype Environment k v = Environment {unEnvironment :: [(k, v)]}
  deriving (Show)

on :: ((k1, v1) -> (k2, v2)) -> Environment k1 v1 -> Environment k2 v2
on f = Environment . fmap f . unEnvironment

empty :: Environment k v
empty = Environment []

lookup :: (Eq k) => k -> Environment k v -> Maybe v
lookup k =
  fmap snd
    . find ((k ==) . fst)
    . unEnvironment

extend :: k -> v -> Environment k v -> Environment k v
extend k v = Environment . ((k, v) :) . unEnvironment
