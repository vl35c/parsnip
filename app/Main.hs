module Main where

import MDParser
import System.Environment (getArgs)


main :: IO [Markdown]
main = do
  [input] <- getArgs
  return $ parse input
