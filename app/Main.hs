module Main where

import Eval
import LispError (extractValue, trapError)
import Parser
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let evaled = fmap show $ readExpr (head args) >>= eval
  putStrLn $ extractValue $ trapError evaled
