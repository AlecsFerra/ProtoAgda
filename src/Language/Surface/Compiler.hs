{-# LANGUAGE FlexibleContexts #-}

module Language.Surface.Compiler (compile) where

import Control.Monad.Reader (MonadReader, asks, local, runReaderT)
import Language.Core.Name (MonadName, fresh, mkDiscarded, mkName)
import qualified Language.Core.Name as C
import qualified Language.Core.Syntax as C
import qualified Language.Environment as E
import qualified Language.Surface.Syntax as S
import Control.Monad (foldM)

-- Association between surface names and core names needed because variables
-- shouldn't geenrate fresh names but reuse the bound ones
type Environment = E.Environment S.Name C.Name

compileTerm :: (MonadReader Environment m, MonadName m) => S.Term -> m C.Term
compileTerm (S.Variable sName) = do
  mCName <- asks $ E.lookup sName
  case mCName of
    Just cName -> pure $ C.Variable cName
    -- The variable is unbound but we will let the typechecker figure it out
    -- so we generate a newname
    Nothing -> C.Variable <$> fresh
compileTerm (S.Lambda sArgName sBody) = do
  cArgName <- mkName sArgName
  let bindName = E.extend sArgName cArgName
  cBody <- local bindName $ compileTerm sBody
  pure $ C.Lambda cArgName cBody
compileTerm (S.Pi sArgName sType sBody) = do
  cType <- compileTerm sType
  cArgName <- mkName sArgName
  let bindName = E.extend sArgName cArgName
  cBody <- local bindName $ compileTerm sBody
  pure $ C.Pi cArgName cType cBody
compileTerm (S.Arrow sType sBody) = do
  cType <- compileTerm sType
  -- We can't refer to the discarded name so who cares about adding it to the
  -- environment
  cBody <- compileTerm sBody
  bogusName <- mkDiscarded
  pure $ C.Pi bogusName cType cBody
compileTerm (S.Application sFun sArg) = do
  cFun <- compileTerm sFun
  cArg <- compileTerm sArg
  pure $ C.Application cFun cArg
compileTerm (S.Annotation sTerm sType) = do
  cTerm <- compileTerm sTerm
  cType <- compileTerm sType
  pure $ C.Annotation cTerm cType
compileTerm S.Universe = pure C.Universe

compileStatement ::
  (MonadName m) =>
  (Environment, S.Statement) ->
  m (Environment, C.Statement)
compileStatement (env, S.Define sName sType sTerm) = do
  cName <- mkName sName
  cTerm <- runReaderT (compileTerm sTerm) env
  cType <- runReaderT (compileTerm sType) env
  let completeTerm = C.Define cName $ C.Annotation cTerm cType
  let env' = E.extend sName cName env
  pure (env', completeTerm)
compileStatement (env, S.Display sTerm) = do
  cTerm <- runReaderT (compileTerm sTerm) env
  pure (env, C.Display cTerm)

compile :: MonadName m => S.Program -> m C.Program
compile program = reverse . snd <$> foldM compileAux (E.empty, []) program
  where
    compileAux (env, rest) sStmt = do
      (env, cStmt) <- compileStatement (env, sStmt)
      return (env, cStmt : rest)


