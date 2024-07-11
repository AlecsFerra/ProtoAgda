module Main (main) where

import Parsing.Parse (parse)
import Language.Check (runProgram)

main :: IO ()
main = do
  src <- readFile "test/id.magda"
  case parse src of
    (Left err) -> print err
    (Right term) -> runProgram term
