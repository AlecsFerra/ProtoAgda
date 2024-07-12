{-# LANGUAGE FlexibleContexts #-}

module Language.Surface.Compiler (compile) where

import Control.Monad (foldM)
import Control.Monad.Reader (MonadReader, asks, local, runReaderT)
import Data.Foldable (foldl')
import Language.Core.Name (MonadName, mkDiscarded, mkName)
import qualified Language.Core.Name as C
import qualified Language.Core.Syntax as C
import qualified Language.Surface.Syntax as S

type NameStack = [(S.Name, C.Name)]

extend :: S.Name -> C.Name -> NameStack -> NameStack
extend s c = ((s, c) :)

makeArgName :: (MonadName m) => String -> m C.Name
makeArgName "_" = mkDiscarded
makeArgName name = mkName name

bindNames :: (MonadName m) => [S.Name] -> m (NameStack -> NameStack, [C.Name])
bindNames sArgNames = do
  cArgNames <- mapM makeArgName sArgNames
  let names = zip sArgNames cArgNames
  -- We bind the names from the first to the last
  let bindNames stack = foldl' (flip $ uncurry extend) stack names
  pure (bindNames, cArgNames)

compileTerm :: (MonadReader NameStack m, MonadName m) => S.Term -> m C.Term
compileTerm (S.Variable "_") = C.Variable <$> mkDiscarded
compileTerm (S.Variable sName) = do
  mCName <- asks $ lookup sName
  case mCName of
    Just cName -> pure $ C.Variable cName
    -- The variable is unbound but we will let the typechecker figure it out
    -- so we generate a newname
    Nothing -> C.Variable <$> mkName sName
compileTerm (S.Lambda sArgNames sBody) = do
  (bindNames, cArgNames) <- bindNames sArgNames
  cBody <- local bindNames $ compileTerm sBody
  -- When we build up the term we start from the last one since we are building
  -- outwards
  pure $ foldr C.Lambda cBody cArgNames
compileTerm (S.Pi sArgNames sType sBody) = do
  cType <- compileTerm sType
  (bindNames, cArgNames) <- bindNames sArgNames
  cBody <- local bindNames $ compileTerm sBody
  -- When we build up the term we start from the last one since we are building
  -- outwards
  pure $ foldr (`C.Pi` cType) cBody cArgNames
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
  (NameStack, S.Statement) ->
  m (NameStack, C.Statement)
compileStatement (env, S.Define sName sType sTerm) = do
  cName <- makeArgName sName
  cTerm <- runReaderT (compileTerm sTerm) env
  cType <- runReaderT (compileTerm sType) env
  let completeTerm = C.Define cName $ C.Annotation cTerm cType
  let env' = extend sName cName env
  pure (env', completeTerm)
compileStatement (env, S.Display sTerm) = do
  cTerm <- runReaderT (compileTerm sTerm) env
  pure (env, C.Display cTerm)

compile :: (MonadName m) => S.Program -> m C.Program
compile program = reverse . snd <$> foldM compileAux ([], []) program
  where
    compileAux (env, rest) sStmt = do
      (env, cStmt) <- compileStatement (env, sStmt)
      return (env, cStmt : rest)
