module LispTypes where

import Control.Monad.Except
import Data.IORef
import GHC.IO.Handle (Handle)
import Text.ParserCombinators.Parsec (ParseError)

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Double
  | String String
  | Bool Bool
  | Character Char
  | Vector [LispVal]
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func {paramemters :: [String], vararg :: Maybe String, body :: [LispVal], closure :: Env}
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

type Env = IORef [(String, IORef LispVal)]

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
showError (Default errMsg) = "Encounter error: " ++ show errMsg

instance Show LispError where show = showError

-- 使用 ExceptT 变换器将 IO 包装起来
type IOThrowsError = ExceptT LispError IO

instance Show LispVal where
  show = showVal

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom typeName) = typeName
showVal (Number contents) = show contents
showVal (Float contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Character contents) = show contents
showVal (Vector contents) = "#(" ++ unwordsList contents ++ ")"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {paramemters = args, vararg = varargs, body = _, closure = _}) =
  "(lambda ("
    ++ unwords (map show args)
    ++ ( case varargs of
           Nothing -> ""
           Just arg -> " . " ++ arg
       )
    ++ ") ...)"
showVal (IOFunc _) = "<IO primitive>"
showVal (Port _) = "<IO port>"
