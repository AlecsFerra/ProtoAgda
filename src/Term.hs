module Term
  ( Term (..),
    Type,
    Statement (..),
    Program,
    alphaEquivalence,
    Difference,
  )
where

import Control.Monad.Except (Except, runExceptT, throwError)
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (ReaderT, asks, local, runReaderT)
import Data.Bifunctor (bimap)
import qualified Environment as E
import Name (FreshNameT, Name, fresh, runFreshNameT)
import Text.Printf (printf)

data Term
  = Variable Name
  | Lambda Name Term
  | Pi Name Type Term
  | Application Term Term
  | Annotation Term Type
  | Universe

type Type = Term

instance Show Term where
  show (Variable name) = show name
  show (Lambda name body) = printf "λ %s. %s" (show name) (show body)
  show (Pi name typ body) = printf "Π (%s : %s) -> %s" (show name) (show typ) (show body)
  show (Application lhs rhs) = printf "(%s %s)" (show lhs) (show rhs)
  show (Annotation term typ) = printf "%s : %s" (show term) (show typ)
  show Universe = "Type"

data Statement
  = Define Name Term
  | Display Term

type Program = [Statement]

data Difference = Difference Term Term
  deriving (Show)

alphaEquivalence :: Term -> Term -> Either Difference ()
alphaEquivalence lhs rhs =
  runIdentity $
    runExceptT $
      runFreshNameT $
        runReaderT (alphaEquivalenceM lhs rhs) (E.empty, E.empty)

type Environment = E.Environment Name Name

type AlphaEquivalenceM = ReaderT (Environment, Environment) (FreshNameT (Except Difference))

extend :: Name -> Name -> Name -> (Environment, Environment) -> (Environment, Environment)
extend lname rname val = bimap (E.extend lname val) (E.extend rname val)

alphaEquivalenceM :: Term -> Term -> AlphaEquivalenceM ()
alphaEquivalenceM l@(Variable lhs) r@(Variable rhs) = do
  mlhs <- asks $ E.lookup lhs . fst
  mrhs <- asks $ E.lookup rhs . snd
  case (mlhs, mrhs) of
    (Nothing, Nothing) | lhs == rhs -> pure ()
    (Just lhs, Just rhs) | lhs == rhs -> pure ()
    _ -> throwError $ Difference l r
alphaEquivalenceM (Lambda larg lbody) (Lambda rarg rbody) = do
  freshId <- fresh
  local (extend larg rarg freshId) $ alphaEquivalenceM lbody rbody
alphaEquivalenceM (Pi larg ltyp lbody) (Pi rarg rtyp rbody) = do
  alphaEquivalenceM ltyp rtyp
  freshId <- fresh
  local (extend larg rarg freshId) $ alphaEquivalenceM lbody rbody
alphaEquivalenceM (Application llhs lrhs) (Application rlhs rrhs) = do
  alphaEquivalenceM llhs rlhs
  alphaEquivalenceM lrhs rrhs
alphaEquivalenceM (Annotation lterm ltyp) (Annotation rterm rtyp) = do
  alphaEquivalenceM ltyp rtyp
  alphaEquivalenceM lterm rterm
alphaEquivalenceM Universe Universe = pure ()
alphaEquivalenceM lhs rhs = throwError $ Difference lhs rhs
