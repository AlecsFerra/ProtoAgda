module Language.Core.Syntax
  ( Term (..),
    Type,
    Statement (..),
    Program,
  )
where

import Language.Core.Name (MName, VName (..))
import Text.Printf (printf)

type Type = Term

data Term
  = Variable VName
  | Meta MName
  | Lambda VName Term
  | Pi VName Type Term
  | Application Term Term
  | Annotation Term Type
  | Universe

data Statement
  = Define VName Term
  | Display Term

type Program = [Statement]

instance Show Term where
  show (Variable name) = show name
  show (Meta name) = show name
  show (Lambda name body) = printf "λ %s. %s" (show name) (show body)
  show (Pi (Discarded _) typ body) = printf "(%s -> %s)" (show typ) (show body)
  show (Pi name typ body) = 
    printf "Π (%s : %s) -> %s" (show name) (show typ) (show body)
  show (Application lhs rhs) = printf "(%s %s)" (show lhs) (show rhs)
  show (Annotation term typ) = printf "%s : %s" (show term) (show typ)
  show Universe = "𝒰"

