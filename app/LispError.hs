{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module LispError where

import Control.Monad.Except
import LispTypes
import Text.ParserCombinators.Parsec (ParseError)

data LispError
  = NumArgs Integer [LispVal] -- 参数个数不对
  | TypeMismatch String LispVal -- 类型不匹配
  | Parser ParseError -- 解析错误
  | BadSpecialForm String LispVal -- 错误的特殊格式
  | NotFunction String String -- 不是一个函数
  | UnboundVar String String -- 未定义的变量
  | Default String -- 其他通用错误

type ThrowsError = Either LispError

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected "
    ++ show expected
    ++ " args; found values "
    ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected "
    ++ expected
    ++ ", found "
    ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
