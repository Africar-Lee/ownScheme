{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parser (readExpr, parseExpr) where

import Control.Monad.Except
import Data.Char (digitToInt)
import LispError
import LispTypes
import Numeric (readHex, readInt, readOct)
import Text.ParserCombinators.Parsec hiding (spaces)

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

-- 解析原子量
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> symbol <|> digit)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom

-- 解析字符字面量
parseCharacter :: Parser LispVal
parseCharacter = do
  _ <- string "#\\"
  value <-
    try (string "space" >> return ' ')
      <|> try (string "newline" >> return '\n')
      <|> anyChar
  return $ Character value

-- 解析转义字符
parseEscapeChar :: Parser Char
parseEscapeChar = do
  _ <- char '\\'
  c <- oneOf "nrt\\\""
  return $ case c of
    'n' -> '\n'
    'r' -> '\r'
    't' -> '\t'
    '\\' -> '\\'
    '\"' -> '\"'

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many $ parseEscapeChar <|> noneOf "\\\""
  _ <- char '"'
  return $ String x

-- 各种进制的解析
parseDecimal :: Parser LispVal
parseDecimal = Number . read <$> many1 digit

parseDecimalExplicit :: Parser LispVal
parseDecimalExplicit = do
  _ <- string "#d"
  parseDecimal

parseHex :: Parser LispVal
parseHex = do
  _ <- string "#x"
  n <- many1 hexDigit
  return $ Number (fst $ head (readHex n))

parseOct :: Parser LispVal
parseOct = do
  _ <- string "#o"
  n <- many1 octDigit
  return $ Number (fst $ head (readOct n))

parseBin :: Parser LispVal
parseBin = do
  _ <- string "#b"
  n <- many1 (oneOf "01")
  let readBin = readInt 2 (`elem` "01") digitToInt
  return $ Number (fst $ head (readBin n))

parseFloat :: Parser LispVal
parseFloat = do
  s <- many1 digit
  _ <- char '.'
  d <- many1 digit
  -- 可选的指数部分
  e <- option "" $ do
    _ <- oneOf "eE"
    optSign <- option "" (string "-")
    ex <- many1 digit
    return $ "e" ++ optSign ++ ex
  return $ Float (read $ s ++ "." ++ d ++ e)

parseNumber :: Parser LispVal
parseNumber =
  try parseDecimalExplicit
    <|> try parseHex
    <|> try parseOct
    <|> try parseBin
    <|> try parseFloat
    <|> parseDecimal

-- 解析列表与点列表
parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  h <- endBy parseExpr spaces
  t <- char '.' >> spaces >> parseExpr
  return $ DottedList h t

-- 解析向量
parseVector :: Parser LispVal
parseVector = do
  _ <- string "#("
  vals <- sepBy parseExpr spaces
  _ <- char ')'
  return $ Vector vals

-- 解析单引号
parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- 解析反引号
parseQuasiquoted :: Parser LispVal
parseQuasiquoted = do
  _ <- char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

-- 解析逗号拼接 ,@
parseUnquoteSplicing :: Parser LispVal
parseUnquoteSplicing = do
  _ <- string ",@"
  x <- parseExpr
  return $ List [Atom "unquote-splicing", x]

-- 解析逗号
parseUnquoted :: Parser LispVal
parseUnquoted = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]

-- 所有表达式解析
parseExpr :: Parser LispVal
parseExpr =
  try parseCharacter
    <|> parseString
    <|> parseNumber
    <|> parseQuoted
    <|> try parseUnquoteSplicing
    <|> parseUnquoted
    <|> parseQuasiquoted
    <|> try parseVector
    <|> parseAtom
    <|> do
      _ <- char '('
      x <- try parseList <|> parseDottedList
      _ <- char ')'
      return x

-- 读出表达式
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val
