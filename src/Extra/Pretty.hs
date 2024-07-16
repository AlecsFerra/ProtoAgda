module Extra.Pretty (Pretty (..)) where

import Language.Core.Name (MName (..), VName (..))
import Language.Core.Syntax (Term (..))
import Language.Core.Value (
  Closure (..), Neutral (..), Normal (..), Value (..), Env (..), MValue (..))
import Text.Printf (printf)
import qualified Data.Map as M
import Data.List (intercalate)

class Pretty a where
  pretty :: a -> String

instance Pretty VName where
  pretty (MachineName id) = show id
  pretty (AnnotatedName ann _) = ann
  pretty (Discarded _) = "_"

instance Pretty MName where
  pretty (MName id) = printf "?%d" id

instance Pretty Term where
  pretty (Variable name) = pretty name
  pretty (Meta name) = pretty name
  pretty (Lambda name body) = printf "λ %s. %s" (pretty name) (pretty body)
  pretty (Pi name typ body) =
    printf "Π (%s : %s) -> %s" (pretty name) (pretty typ) (pretty body)
  pretty (Application lhs rhs) = printf "(%s %s)" (pretty lhs) (pretty rhs)
  pretty (Annotation lhs rhs) = printf "(%s : %s)" (pretty lhs) (pretty rhs)
  pretty Universe = "𝒰"

instance Pretty Neutral where
  pretty (NApplication fun arg) = printf "(%s %s)" (pretty fun) (pretty arg)
  pretty (NVariable name) = pretty name

instance Pretty Value where
  pretty (VPi argType (Closure _ (Discarded _) body)) =
    printf "%s → %s" (pretty argType) (pretty body)
  pretty (VPi argType (Closure _ argName body)) =
    printf "Π (%s : %s) -> %s" (pretty argName) (pretty argType) (pretty body)
  pretty (VLambda (Closure _ argName body)) =
    printf "λ %s. %s" (pretty argName) (pretty body)
  pretty VUniverse = "𝒰"
  pretty (VNeutral _ term) = pretty term
  pretty (VMeta name) = pretty name

instance Pretty Normal where
  pretty (NAnnotated _ term) = pretty term

instance (Pretty k, Pretty v) => Pretty (Env k v) where
  pretty = intercalate "," . fmap kvs . M.toList . unEnv
    where kvs (k, v) = printf "%s: %s" (pretty k) (pretty v)

instance Pretty MValue where
  pretty Unsolved = "Unsolved"
  pretty (Solved val) = pretty val
