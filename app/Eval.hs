{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Eval where

import LispTypes

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Bool _) = val
eval val@(Vector _) = val
eval val@(Character _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval val@(Atom _) = val

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
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

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp f [v] = f v
unaryOp _ _ = Bool False

integerBinOp :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
integerBinOp op params = Number $ foldl1 op $ map unpackInt params

numericBinOp ::
  (Integer -> Integer -> Integer) ->
  (Double -> Double -> Double) ->
  [LispVal] ->
  LispVal
numericBinOp intOp floatOp params =
  if anyFloats params
    then Float $ foldl1 floatOp $ map unpackFloat params
    else Number $ foldl1 intOp $ map unpackInt params

boolBinOp :: (LispVal -> a) -> (a -> a -> Bool) -> [LispVal] -> LispVal
boolBinOp unpacker op params = Bool $ and $ zipWith op unpacked (tail unpacked)
  where
    unpacked = map unpacker params

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
unpackInt :: LispVal -> Integer
unpackInt (Number n) = n
unpackInt (String n) =
  let parsed = reads n :: [(Integer, String)]
   in if null parsed
        then 0
        else fst $ head parsed
unpackInt (List [n]) = unpackInt n
unpackInt _ = 0

anyFloats :: [LispVal] -> Bool
anyFloats = any $ unpackBool . isFloat

unpackFloat :: LispVal -> Double
unpackFloat (Float n) = n
unpackFloat (Number n) = fromIntegral n
unpackFloat (String n) = case reads n :: [(Double, String)] of
  [(val, _)] -> val
  _ -> 0
unpackFloat (List [n]) = unpackFloat n
unpackFloat _ = 0

unpackStr :: LispVal -> String
unpackStr (String s) = s
unpackStr (Number s) = show s
unpackStr (Bool s) = show s
unpackStr _ = ""

unpackBool :: LispVal -> Bool
unpackBool (Bool b) = b
unpackBool _ = False
