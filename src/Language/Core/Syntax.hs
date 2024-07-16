module Language.Core.Syntax
  ( Term (..),
    Type,
    Statement (..),
    Program,
  )
where

import Language.Core.Name (MName, VName (..))

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

