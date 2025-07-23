{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Main where

import Data.Char (isAlpha, isDigit)
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)

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

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
-- unpackNum (String n) =
--     let parsed = reads n :: [(Integer, String)]
--      in if null parsed
--             then 0
--             else fst $ head parsed
-- unpackNum (List [n]) = unpackNum n
unpackNum _ = 0

typeTestop :: String -> [LispVal] -> LispVal
typeTestop "string" ((String _) : _) = Bool True
typeTestop "number" ((Number _) : _) = Bool True
typeTestop "symbol" ((Atom symm) : _) = if testAllCh symm then Bool True else Bool False
  where
    testAllCh (c : rest) = (elem c "!$%&*+-./:<=>?@^_~" || isAlpha c) && testAllCh' rest
      where
        testAllCh' (c' : rest') = (elem c' "!$%&*+-./:<=>?@^_~" || isAlpha c' || isDigit c') && testAllCh' rest'
        testAllCh' [] = True
    testAllCh [] = False
typeTestop _ _ = Bool False

sym2str :: [LispVal] -> LispVal
sym2str ((Atom symm) : _) = String symm

str2sym :: [LispVal] -> LispVal
str2sym ((String str) : _) = Atom str

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

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

primitives :: [(String, [LispVal] -> LispVal)]
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

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
