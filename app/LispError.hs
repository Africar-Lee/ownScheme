{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module LispError where

import Control.Monad.Except
import LispTypes (IOThrowsError, ThrowsError)

-- import Text.ParserCombinators.Parsec (ParseError)

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- 辅助函数：将纯错误转换为 IO 错误
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

-- 辅助函数：运行并捕获 IO 错误
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = do
  result <- runExceptT (trapError action)
  return $ extractValue result
