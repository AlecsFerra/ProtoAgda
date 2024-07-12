module Main (main) where

import Parsing.Parse (parse)
import Language.Core.Engine (runProgram)
import Language.Surface.Compiler (compile)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Language.Core.Name (MonadName, runNameT)

runM :: (MonadIO m, MonadName m) => String -> m ()
runM path = do
  src <- liftIO $ readFile path
  let parsed = parse src
  case parsed of
    Left error -> do 
      liftIO $ print error
    Right parsed -> do
      compiled <- compile parsed
      runProgram compiled

run :: String -> IO ()
run path = runNameT $ runM path

main :: IO ()
main = do
  run "./test/exampleDiscard.magda"
