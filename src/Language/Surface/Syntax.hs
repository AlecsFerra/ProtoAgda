module Language.Surface.Syntax (Term (..), Statement (..), Program, Name) where

type Name = String

type Type = Term

data Term
  = -- variables
    Variable Name
  | -- λ x₁ ... xₙ. t
    Lambda [Name] Term
  | -- Π (x₁ ... xₙ : t₁) -> t₂
    Pi [Name] Type Term
  | -- t₁ -> t₂
    Arrow Type Term
  | -- (t₁ t₂)
    Application Term Term
  | -- (t₁ : t₂)
    Annotation Term Type
  | -- 𝒰
    Universe

data Statement
  = Define Name Type Term
  | Display Term

type Program = [Statement]

