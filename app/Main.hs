module Main where

-- import LispEnv

import Data.Char (isSpace)
import Eval
import LispEnv (bindVar)
import LispError
import LispTypes
import Parser
import System.Environment
import System.IO

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runRepl
    else runOne args

runOne :: [String] -> IO ()
runOne args = do
  env <- primitiveBindings >>= flip bindVar [("args", List $ map String $ drop 1 args)]
  runIOThrows (show <$> eval env (List [Atom "load", String (head args)])) >>= hPutStr stderr

runRepl :: IO ()
runRepl = do
  env <- primitiveBindings
  until_ (== "quit") (readPrompt "Lisp >>> ") $ \input ->
    if null input || all isSpace input
      then return ()
      else evalAndPrint env input

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftThrows (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: (Monad m) => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ condition prompt action = do
  result <- prompt
  if condition result
    then return ()
    else action result >> until_ condition prompt action
