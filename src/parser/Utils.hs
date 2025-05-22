{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use camelCase" #-}

module Utils where

import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Monad (void, zipWithM)
import Data.Functor (($>))
import Data.Char (toLower)
import Data.Maybe (listToMaybe, mapMaybe)

import AST
import AST (ConstraintType(Unique), SQLValue)

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p <* spaces

identifier :: Parser String
identifier = lexeme (many1 (letter <|> digit <|> char '_'))

stringCI :: String -> Parser String
stringCI s = try $ do
    let matchChar target input = if toLower target == toLower input then return input else fail "Error: Invalid SQL Statement"
    zipWithM matchChar s =<< count (length s) anyChar

parseValue :: Parser SQLValue
parseValue = lexeme (try parseNull
         <|> try parseDate
         <|> try parseFloat
         <|> try parseInt
         <|> try parseString
         <|> try parseBool)

parseNull :: Parser SQLValue
parseNull = stringCI "null" $> SQLNull

parseString :: Parser SQLValue
parseString = lexeme $ do
    char '\''
    str <- manyTill anyChar (char '\'')
    return $ SQLString str

parseInt :: Parser SQLValue
parseInt = lexeme (SQLInt . read <$> many1 digit)

parseFloat :: Parser SQLValue
parseFloat = lexeme $ do
    num <- many1 digit
    char '.'
    frac <- many1 digit
    return $ SQLFloat (read (num ++ "." ++ frac))

parseDate :: Parser SQLValue
parseDate = lexeme $ do
    char '\''
    day <- count 2 digit
    char '-'
    month <- count 2 digit
    char '-'
    year <- count 4 digit
    char '\''
    return $ SQLDate (day ++ "-" ++ month ++ "-" ++ year)

parseBool :: Parser SQLValue
parseBool = lexeme $
    (try (stringCI "true") $> SQLBool True)
    <|> (try (stringCI "false") $> SQLBool False)

parseConstraint :: Parser ConstraintType
parseConstraint = lexeme $ choice
    [ try (stringCI "PRIMARY KEY" $> PrimaryKey)
    , try (stringCI "FOREIGN KEY" $> ForeignKey)
    , try (stringCI "NOT NULL" $> NotNull)
    , try (stringCI "UNIQUE" $> Unique)
    ]

parseDataType :: Parser DataType
parseDataType = lexeme $ choice
    [ try (stringCI "INT" $> IntType)
    , try (stringCI "FLOAT" $> FloatType)
    , try parseVarChar
    , try (stringCI "DATE" $> DateType)
    , try (stringCI "BOOL" $> BoolType)
    ]

parseVarChar :: Parser DataType
parseVarChar = lexeme $ do
    void $ stringCI "VARCHAR"
    size <- between (char '(') (char ')') (read <$> many1 digit)
    if size > 0 && size <= 255
        then return (VarCharType size)
        else fail "Error: VARCHAR size must be between 1 and 255"

parseColumn :: Parser (String, DataType, Maybe ConstraintType)
parseColumn = lexeme $ do
    colName <- identifier
    colType <- parseDataType
    colConstraint <- optionMaybe parseConstraint
    return (colName, colType, colConstraint)

parseAssignment :: Parser (String, String)
parseAssignment = lexeme $ do
    colName <- identifier
    void $ lexeme (char '=')
    value <- parseValue
    return (colName, show value)

parseCondition :: Parser Condition
parseCondition = lexeme parseOrCondition

parseOrCondition :: Parser Condition
parseOrCondition = chainl1 parseAndCondition orOperator

parseAndCondition :: Parser Condition
parseAndCondition = chainl1 parseBaseCondition andOperator

parseBaseCondition :: Parser Condition
parseBaseCondition = lexeme (try parseComparison <|> between (lexeme (char '(')) (lexeme (char ')')) parseCondition)

parseComparison :: Parser Condition
parseComparison = do
    col <- identifier
    op <- lexeme (choice
        [ try (string ">=" $> GreaterOrEqual)
        , try (string "<=" $> LessOrEqual)
        , try (string "!=" $> NotEquals)
        , try (string "="  $> Equals)
        , try (string ">"  $> GreaterThan)
        , try (string "<"  $> LessThan)
        ])
    val <- parseValue
    return (op col val)

andOperator :: Parser (Condition -> Condition -> Condition)
andOperator = lexeme (stringCI "AND") $> And

orOperator :: Parser (Condition -> Condition -> Condition)
orOperator = lexeme (stringCI "OR") $> Or
