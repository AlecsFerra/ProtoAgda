module Main (main) where

import Parsing.Parse (parse)
import Language.Core.Engine (runProgram)
import Language.Surface.Compiler (compile)
import Language.Core.Name (runNameT)
import System.Environment (getArgs)

run :: String -> IO ()
run src = do
  let parsed = parse src
  case parsed of
    Left error -> do 
      print error
    Right parsed -> do
      runNameT $ compile parsed >>= runProgram

runFile :: String -> IO ()
runFile path = do
  src <- readFile path
  run src

main :: IO ()
main = do
  args <- getArgs
  case args of
    [fileName] -> runFile fileName
    _ -> print "Wrong number of arguments"
  
