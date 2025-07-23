{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Control.Monad.Except
import Data.Char (isAlpha, isDigit)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

{- Error handling -}
data LispError
    = NumArgs Integer [LispVal]
    | TypeMismatch String LispVal
    | Parser ParseError
    | BadSpecialForm String LispVal
    | NotFunction String String
    | UnboundVar String String
    | Default String

instance Show LispError where
    show (UnboundVar message varname) = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func) = message ++ ": " ++ show func
    show (NumArgs expected found) =
        "Expected "
            ++ show expected
            ++ " args; found values "
            ++ unwordsList found
    show (TypeMismatch expected found) =
        "Invalid type: expected "
            ++ expected
            ++ ", found "
            ++ show found
    show (Parser parseErr) = "Parse error at " ++ show parseErr

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

{- LispVal type defining -}
data LispVal
    = Atom String
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | Rational Integer Integer
    | Float Double
    | Complex Double Double
    | String String
    | Bool Bool
    | Character Char

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

instance Show LispVal where
    show (String s) = "\"" ++ s ++ "\""
    show (Character c) = show [c]
    show (Number i) = show i
    show (Rational a b) = show a ++ "/" ++ show b
    show (Float f) = show f
    -- 自定义 Show 实例以匹配 "a+bi" 的格式
    show (Complex r i) = show r ++ "+" ++ show i ++ "i"
    show (Atom atom) = show atom
    show (List contentz) = "(" ++ unwordsList contentz ++ ")"
    show (DottedList h t) = "(" ++ unwordsList h ++ show t ++ ")"
    show (Bool True) = "#t"
    show (Bool False) = "#f"

{- Primitives defining -}
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
    let parsed = reads n
     in if null parsed
            then throwError $ TypeMismatch "number" $ String n
            else return $ fst $ head parsed
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

typeTestop :: String -> [LispVal] -> ThrowsError LispVal
typeTestop "string" [String _] = return $ Bool True
typeTestop "number" [Number _] = return $ Bool True
typeTestop "symbol" [Atom symm] = if testAllCh symm then return $ Bool True else return $ Bool False
  where
    testAllCh (c : rest) = (elem c "!$%&*+-./:<=>?@^_~" || isAlpha c) && testAllCh' rest
      where
        testAllCh' (c' : rest') = (elem c' "!$%&*+-./:<=>?@^_~" || isAlpha c' || isDigit c') && testAllCh' rest'
        testAllCh' [] = True
    testAllCh [] = False
typeTestop _ [] = throwError $ NumArgs 1 []
typeTestop _ _ = return $ Bool False

sym2str :: [LispVal] -> ThrowsError LispVal
sym2str [] = throwError $ NumArgs 1 []
sym2str [Atom symm] = return $ String symm
sym2str params = throwError $ NumArgs 1 params

str2sym :: [LispVal] -> ThrowsError LispVal
str2sym [] = throwError $ NumArgs 1 []
str2sym [String str] = return $ Atom str
str2sym params = throwError $ NumArgs 1 params

{- Parsing -}
spaces :: Parser ()
spaces = skipMany1 space

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

escChar :: Parser Char
escChar =
    do
        _ <- char '\\'
        c <- oneOf "\\\"nrt"
        return $ case c of
            'n' -> '\n'
            't' -> '\t'
            'r' -> '\r'
            '\"' -> '"'
            '\\' -> '\\'

parseString :: Parser LispVal
parseString = do
    x <- between (char '"') (char '"') (many (escChar <|> noneOf "\"\\"))
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> symbol <|> digit)
    let atom = first : rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _ -> Atom atom

parseChar :: Parser LispVal
parseChar = do
    _ <- char '#'
    _ <- char '\\'
    c <-
        try (string "newline" >> return '\n')
            <|> try (string "space" >> return ' ')
            <|> anyChar
    return $ Character c

parseInteger :: Parser LispVal
parseInteger = Number . read <$> many1 digit

parseRational :: Parser LispVal
parseRational = do
    num <- many1 digit
    _ <- char '/'
    den <- many1 digit
    return $ Rational (read num) (read den)

parseFloat :: Parser LispVal
parseFloat = do
    whole <- many1 digit
    _ <- char '.'
    frac <- many digit
    return $ Float (read (whole ++ "." ++ frac))

-- 辅助解析器：解析一个数字（整数或浮点数）并返回 Double
parseNumberAsDouble :: Parser Double
parseNumberAsDouble = do
    -- 必须首先尝试解析浮点数，因为整数是浮点数的前缀
    -- 例如，在 "123.45" 中，如果先尝试整数，它会匹配 "123" 并停止。
    val <- try (show <$> parseFloat') <|> many1 digit
    return (read val)
  where
    -- 解析浮点数并返回其字符串表示
    parseFloat' :: Parser String
    parseFloat' = do
        whole <- many1 digit
        _ <- char '.'
        frac <- many1 digit
        return (whole ++ "." ++ frac)

parseComplex :: Parser LispVal
parseComplex = do
    realPart <- parseNumberAsDouble
    _ <- char '+'
    imagPart <- parseNumberAsDouble
    _ <- char 'i'
    return $ Complex realPart imagPart

parseNumber :: Parser LispVal
parseNumber =
    try parseComplex
        <|> try parseFloat
        <|> try parseRational
        <|> parseInteger

-- Using spaces to seperate expression, and lift them into a List
parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    h <- endBy parseExpr spaces
    t <- char '.' >> spaces >> parseExpr
    return $ DottedList h t

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
    _ <- char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
    _ <- try (string ",@")
    x <- parseExpr
    return $ List [Atom "unquote-splicing", x]

parseUnquote :: Parser LispVal
parseUnquote = do
    _ <- char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseExpr :: Parser LispVal
parseExpr =
    try parseChar
        <|> parseString
        <|> parseAtom
        <|> parseNumber
        <|> parseQuoted
        <|> parseQuasiquote
        <|> try parseUnquoteSplicing
        <|> parseUnquote
        <|> do
            _ <- char '('
            x <- try parseList <|> parseDottedList
            _ <- char ')'
            return x

{- REPL -}
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
    [ ("+", numericBinop (+))
    , ("-", numericBinop (-))
    , ("*", numericBinop (*))
    , ("/", numericBinop div)
    , ("mod", numericBinop mod)
    , ("quotient", numericBinop quot)
    , ("remainder", numericBinop rem)
    , ("string?", typeTestop "string")
    , ("number?", typeTestop "number")
    , ("symbol?", typeTestop "symbol")
    , ("symbol->string", sym2str)
    , ("string->symbol", str2sym)
    ]

apply :: String -> [LispVal] -> ThrowsError LispVal
-- apply func args = maybe (Bool False) ($ args) $ lookup func primitives
apply func args =
    maybe
        (throwError $ NotFunction "Unrecognized primitive function args" func)
        ($ args)
        (lookup func primitives)

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

main :: IO ()
main = do
    args <- getArgs
    let evaled = fmap show $ readExpr (head args) >>= eval
    putStrLn $ extractValue $ trapError evaled
