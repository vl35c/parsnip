module Main where

import MDParser
import System.Environment (getArgs)


main :: IO ()
main = do
  [input] <- getArgs
  putStrLn $ show $ parse input
