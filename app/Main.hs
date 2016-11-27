module Main where

import System.Environment
import Runner

main :: IO ()
main = do
  simulationType <- getArgs
  runner (head simulationType)
