module Main (main) where

import Parsing.Parse (parse)
import Language.Core.Engine (runProgram)
import Language.Surface.Compiler (compile)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Language.Core.Name (MonadName, runNameT)

pipeline :: (MonadIO m, MonadName m) => String -> m ()
pipeline src = do
  let parsed = parse src
  case parsed of
    Left error -> do 
      liftIO $ print error
    Right parsed -> do
      compiled <- compile parsed
      runProgram compiled

main :: IO ()
main = do
  src <- readFile "test/idArrowty.magda"
  runNameT $ pipeline src
