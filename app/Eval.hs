{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{- HLINT ignore "Use <&>" -}

module Eval where

import Control.Monad.Except
import LispError
import LispTypes

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Bool _) = return val
eval val@(Vector _) = return val
eval val@(Character _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval val@(Atom _) = return val
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe
    (throwError $ NotFunction "Unrecognized primitive function args" func)
    ($ args)
    (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinOp (+) (+)),
    ("-", numericBinOp (-) (-)),
    ("*", numericBinOp (*) (*)),
    ("/", numericBinOp div (/)),
    ("mod", integerBinOp mod),
    ("quotient", integerBinOp quot),
    ("remainder", integerBinOp rem),
    -- 数值比较
    ("=", boolBinOp unpackFloat (==)),
    ("<", boolBinOp unpackFloat (<)),
    (">", boolBinOp unpackFloat (>)),
    ("/=", boolBinOp unpackFloat (/=)),
    (">=", boolBinOp unpackFloat (>=)),
    ("<=", boolBinOp unpackFloat (<=)),
    -- 布尔逻辑
    ("&&", boolBinOp unpackBool (&&)),
    ("||", boolBinOp unpackBool (||)),
    -- 字符串比较
    ("string=?", boolBinOp unpackStr (==)),
    ("string<?", boolBinOp unpackStr (<)),
    ("string>?", boolBinOp unpackStr (>)),
    ("string<=?", boolBinOp unpackStr (<=)),
    ("string>=?", boolBinOp unpackStr (>=)),
    -- 类型判断
    ("symbol?", unaryOp isSymbol),
    ("string?", unaryOp isString),
    ("number?", unaryOp isNumber),
    ("bool?", unaryOp isBool),
    ("pair?", unaryOp isPair),
    ("list?", unaryOp isList),
    ("vector?", unaryOp isVector),
    ("char?", unaryOp isChar),
    ("float?", unaryOp isFloat),
    -- 符号处理
    ("symbol->string", unaryOp sym2str),
    ("string->symbol", unaryOp str2sym)
  ]

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp _ [] = throwError $ NumArgs 1 []
unaryOp f [v] = return $ f v
unaryOp _ [_, _] = throwError $ NumArgs 1 []

integerBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
integerBinOp _ [] = throwError $ NumArgs 2 []
integerBinOp _ singleval@[_] = throwError $ NumArgs 2 singleval
integerBinOp op params = mapM unpackInt params >>= return . Number . foldl1 op

numericBinOp ::
  (Integer -> Integer -> Integer) ->
  (Double -> Double -> Double) ->
  [LispVal] ->
  ThrowsError LispVal
numericBinOp _ _ [] = throwError $ NumArgs 2 []
numericBinOp _ _ singleval@[_] = throwError $ NumArgs 2 singleval
numericBinOp intOp floatOp params =
  if anyFloats params
    then mapM unpackFloat params >>= return . Float . foldl1 floatOp
    else mapM unpackInt params >>= return . Number . foldl1 intOp

boolBinOp :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinOp _ _ [] = throwError $ NumArgs 2 []
boolBinOp _ _ singleval@[_] = throwError $ NumArgs 2 singleval
boolBinOp unpacker op params = do
  unpacked <- mapM unpacker params
  return . Bool . and $ zipWith op unpacked (tail unpacked)

isSymbol, isString, isNumber, isBool, isList, isPair, isVector, isChar, isFloat :: LispVal -> LispVal
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False
isString (String _) = Bool True
isString _ = Bool False
-- 在 Scheme 中，整数和浮点数都属于 number?
isNumber (Number _) = Bool True
isNumber (Float _) = Bool True
isNumber _ = Bool False
isFloat (Float _) = Bool True
isFloat _ = Bool False
isBool (Bool _) = Bool True
isBool _ = Bool False
isChar (Character _) = Bool True
isChar _ = Bool False
isVector (Vector _) = Bool True
isVector _ = Bool False
-- pair? 指的是任何由 cons 构成的结构（非空列表或点列表）
isPair (List (_ : _)) = Bool True
isPair (DottedList _ _) = Bool True
isPair _ = Bool False
-- list? 指的是标准的、以 nil 结尾的列表
isList (List _) = Bool True
isList _ = Bool False

sym2str :: LispVal -> LispVal
sym2str (Atom content) = String content
sym2str v = v

str2sym :: LispVal -> LispVal
str2sym (String content) = Atom content
str2sym v = v

-- utils
unpackInt :: LispVal -> ThrowsError Integer
unpackInt (Number n) = return n
unpackInt (List [n]) = unpackInt n
unpackInt notNum = throwError $ TypeMismatch "number" notNum

anyFloats :: [LispVal] -> Bool
anyFloats = any $ getBool . isFloat
  where
    getBool (Bool True) = True
    getBool (Bool False) = False

unpackFloat :: LispVal -> ThrowsError Double
unpackFloat (Float n) = return n
unpackFloat (Number n) = return $ fromIntegral n
unpackFloat (List [n]) = unpackFloat n
unpackFloat notNum = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr notStr = throwError $ TypeMismatch "string" notStr

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool
