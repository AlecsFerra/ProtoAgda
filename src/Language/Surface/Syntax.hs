module Language.Surface.Syntax (Term) where

import Language.Name (Name)

type Type = Term

data Term
  = -- variables
    Variable Name
    -- λ x . r
  | Lambda Name Term
    -- Π (x : t₁) -> t₂  (Maybe it's set)
    -- t₁ -> t₂          (Maybe it's not set)
  | Pi (Maybe Name) Type Term
    -- (t₁ t₂)
  | Application Term Term
    -- (t₁ : t₂)
  | Annotation Term Type
    -- 𝒰
  | Universe

