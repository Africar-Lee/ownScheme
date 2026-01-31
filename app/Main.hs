module Main where

import Eval
import LispError (extractValue, trapError)
import Parser
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case length args of
    0 -> runRepl
    1 -> evalAndPrint $ head args
    _ -> putStrLn "Program takes only 0 or 1 argument"

runRepl :: IO ()
runRepl = until_ (== "quit") (readPrompt "Lisp >>> ") $ \input ->
  if null input
    then return () -- 如果是空字符串，什么都不做，直接返回。until_ 会继续下一轮循环。
    else evalAndPrint input

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (fmap show $ readExpr expr >>= eval)

evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

until_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ condition prompt action = do
  result <- prompt
  if condition result
    then return ()
    else action result >> until_ condition prompt action
