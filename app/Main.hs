module Main where

import MDParser
import PythonParser
import System.Environment (getArgs)


main :: IO ()
main = do
  [input, file_type] <- getArgs
  case file_type of
    "md" -> putStrLn $ show $ parse_md input
    "py" -> putStrLn $ show $ parse_py input
    _ -> putStrLn ""
