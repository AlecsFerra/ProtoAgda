module Language.Environment
  ( Environment,
    empty,
    lookup,
    extend,
    mapWithKey,
  )
where

import qualified Data.Map as M
import Prelude hiding (lookup)

-- Names are unique so we don't have to keep track of the scope length,
-- this makes a lot of things easier for example the scoping of types
newtype Environment k v = Environment {unEnvironment :: M.Map k v}
  deriving (Show)

mapWithKey :: (k -> v1 -> v2) -> Environment k v1 -> Environment k v2
mapWithKey f = Environment . M.mapWithKey f . unEnvironment

empty :: Environment k v
empty = Environment M.empty

lookup :: (Ord k) => k -> Environment k v -> Maybe v
lookup k = M.lookup k . unEnvironment

extend :: (Ord k) => k -> v -> Environment k v -> Environment k v
extend k v = Environment . M.insert k v . unEnvironment
