{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

{- HLINT ignore "Use <&>" -}

module Eval where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Maybe (isNothing)
import LispEnv
import LispError
import LispTypes
import Parser (readExpr, readExprList)
import System.IO

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Float _) = return val
eval _ val@(Bool _) = return val
eval env (Atom name) = getVar env name
eval _ val@(Vector _) = return val
eval _ val@(Character _) = return val
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", condition, conseq, alt]) = do
  result <- eval env condition
  case result of
    Bool False -> eval env alt
    Bool True -> eval env conseq
    _ -> throwError $ TypeMismatch "Boolean" condition
eval env (List (Atom "cond" : clauses)) = do
  case clauses of
    [] -> throwError $ NumArgs 1 []
    ((List [test, expr]) : cs) ->
      if test == Atom "else"
        then
          eval env expr
        else do
          checkcond <- eval env test
          case checkcond of
            Atom "else" -> eval env expr
            Bool True -> eval env expr
            Bool False -> eval env (List (Atom "cond" : cs))
    _ -> throwError $ BadSpecialForm "(cond ((<test>) <expr>))" (head clauses)
eval env (List (Atom "case" : key : clauses)) = do
  case clauses of
    [] -> return $ Atom "unspecified"
    (List [List xs, expr] : cs) -> do
      res <- eval env key
      if res `elem` xs
        then eval env expr
        else eval env (List (Atom "case" : key : cs))
    _ -> throwError $ BadSpecialForm "(case <key> ((<test1>) <expr1>)..)" (head clauses)
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarArgs varargs env [] body
eval env (List [Atom "load", String filename]) = load filename >>= fmap last . mapM (eval env)
eval env (List (function : args)) = do
  func <- eval env function
  argVals <- mapM (eval env) args
  apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (IOFunc func) args = func args
apply (Func params varargs body closure) args =
  if num params /= num args && isNothing varargs
    then throwError $ NumArgs (num params) args
    else liftIO (bindVar closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
  where
    remainingArgs = drop (length params) args
    num = toInteger . length
    evalBody env = last <$> mapM (eval env) body
    bindVarArgs vars env = case vars of
      Just argName -> liftIO $ bindVar env [(argName, List remainingArgs)]
      Nothing -> return env

makeFunc :: (Monad m) => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env params body =
  return $ Func (map showVal params) varargs body env

makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeNormalFunc = makeFunc Nothing

makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> ExceptT LispError IO LispVal
makeVarArgs = makeFunc . Just . showVal

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("apply", applyProc),
    ("open-input-file", makePort ReadMode),
    ("open-output-file", makePort WriteMode),
    ("close-input-file", closePort),
    ("close-output-file", closePort),
    ("read", readProc),
    ("write", writeProc),
    ("read-contents", readContents),
    ("read-all", readAll)
  ]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args) = apply func args

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = fmap Port $ liftIO $ openFile filename mode

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> return (Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = liftIO (hGetLine port) >>= liftThrows . readExpr

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> return (Bool True)

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = String <$> liftIO (readFile filename)

load :: String -> IOThrowsError [LispVal]
load filename = liftIO (readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = List <$> load filename

primitiveBindings :: IO Env
primitiveBindings =
  nullEnv
    >>= flip
      bindVar
      ( map (makeFunc IOFunc) ioPrimitives
          ++ map (makeFunc PrimitiveFunc) primitives
      )
  where
    makeFunc constructor (var, func) = (var, constructor func)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinOp (+) (+)),
    ("-", numericBinOp (-) (-)),
    ("*", numericBinOp (*) (*)),
    ("/", numericBinOp div (/)),
    ("mod", integerBinOp mod),
    ("quotient", integerBinOp quot),
    ("remainder", integerBinOp rem),
    -- 一般比较
    ("eqv?", eqv),
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
    ("string->symbol", unaryOp str2sym),
    -- 列表处理
    ("cons", cons),
    ("car", car),
    ("cdr", cdr)
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

-- primitives
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

cons :: [LispVal] -> ThrowsError LispVal
cons [v, List []] = return $ List [v]
cons [v, List vs] = return $ List (v : vs)
cons [v, DottedList [] d] = return $ DottedList [v] d
cons [v, DottedList vs d] = return $ DottedList (v : vs) d
cons [v1, v2] = return $ DottedList [v1] v2
cons badArgList = throwError $ NumArgs 2 badArgList

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)] = return x
car [DottedList (x : _) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Float arg1, Float arg2] = return $ Bool $ arg1 == arg2
eqv [Float arg1, Number arg2] = return $ Bool $ arg1 == fromIntegral arg2
eqv [Number arg1, Float arg2] = return $ Bool $ fromIntegral arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [List arg1, List arg2] =
  return $
    Bool $
      (length arg1 == length arg2)
        && all eqvPair (zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
      Left _ -> False
      Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

instance Eq LispVal where
  (==) a b = case eqv [a, b] of
    Right (Bool True) -> True
    Right (Bool False) -> False
    _ -> False

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
