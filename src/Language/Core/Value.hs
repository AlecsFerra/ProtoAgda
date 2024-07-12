module Language.Core.Value
  ( Environment,
    Context,
    VType,
    Value (..),
    Closure (..),
    Normal (..),
    Bound (..),
    Neutral (..),
  )
where

import Language.Core.Name
import Language.Core.Syntax
import qualified Language.Environment as E

type Environment = E.Environment Name Value

type Context = E.Environment Name Bound

type VType = Value

data Value
  = VPi VType Closure
  | VLambda Closure
  | VUniverse
  | -- We need to keep track of the type of neutral term
    VNeutral VType Neutral

data Closure = Closure Environment Name Term

data Normal
  = -- Normal forms are values annotated with a type
    NAnnotated VType Value

-- Neutral expressions are expression that are temporarly stuck
data Neutral
  = -- A function application is stuck if we don't
    -- know the function that we are applying, the
    -- argument must be in normal form, meaning we
    -- can't reduce it further
    NApplication Neutral Normal
  | -- A variable that is nout bound in this context
    NVariable Name

data Bound
  = Definition VType Value
  | Bind VType


-- How a specific name should be displayed
-- 
type NameStyle = E.Environment Name String

prettyPrint :: (MonadState )

instance Show Value where
  show v = showAux [] v

  where
    showAux
