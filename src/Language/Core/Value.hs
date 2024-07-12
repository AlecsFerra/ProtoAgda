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
import Text.Printf (printf)

type Environment = E.Environment Name Value

type Context = E.Environment Name Bound

type VType = Value

data Value
  = VPi VType Closure
  | VLambda Closure
  | VUniverse
  | -- We need to keep track of the type of neutral term
    VNeutral VType Neutral

instance Show Value where
  show (VPi argType (Closure _ (Discarded _) body)) =
    printf "%s → %s" (show argType) (show body)
  show (VPi argType (Closure _ argName body)) =
    printf "Π (%s : %s) -> %s" (show argName) (show argType) (show body)
  show (VLambda (Closure _ argName body)) =
    printf "λ %s. %s" (show argName) (show body)
  show VUniverse = "𝒰"
  show (VNeutral _ term) = show term

data Closure = Closure Environment Name Term
  deriving (Show)

data Normal
  = -- Normal forms are values annotated with a type
    NAnnotated VType Value

instance Show Normal where
  show (NAnnotated _ term) = show term

-- Neutral expressions are expression that are temporarly stuck
data Neutral
  = -- A function application is stuck if we don't
    -- know the function that we are applying, the
    -- argument must be in normal form, meaning we
    -- can't reduce it further
    NApplication Neutral Normal
  | -- A variable that is nout bound in this context
    NVariable Name

instance Show Neutral where
  show (NApplication fun arg) = printf "(%s %s)" (show fun) (show arg)
  show (NVariable name) = show name

data Bound
  = Definition VType Value
  | Bind VType
  deriving (Show)
