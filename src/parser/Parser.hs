{-# LANGUAGE OverloadedStrings #-}

module Parser (parseSQL) where

import Text.Parsec
import Control.Monad (void, zipWithM)
import Data.Functor (($>))
import Text.Parsec.String (Parser)
import Text.Parsec.Char (string)
import Data.Char (toLower)

import AST
import Utils

parseSelect :: Parser SQLStatement
parseSelect = lexeme $ do
    void $ lexeme (stringCI "select")
    cols <- try (lexeme (string "*") $> ["*"]) <|> sepBy identifier (lexeme (char ','))
    void $ lexeme (stringCI "from")
    table <- identifier
    cond <- optionMaybe parseWhere
    return $ SelectStmt cols table cond

parseCreate :: Parser SQLStatement
parseCreate = lexeme $ do
    void $ lexeme (stringCI "create")
    void $ lexeme (stringCI "table")
    table <- identifier
    cols <- between (lexeme (char '(')) (lexeme (char ')')) (sepBy parseColumn (lexeme (char ',')))
    return $ CreateStmt table cols

parseInsert :: Parser SQLStatement
parseInsert = lexeme $ do
    void $ lexeme (stringCI "insert")
    void $ lexeme (stringCI "into")
    table <- identifier
    cols <- optionMaybe $ try (between (lexeme (char '(')) (lexeme (char ')')) (sepBy identifier (lexeme (char ','))))
    void $ lexeme (stringCI "values")
    values <- between (lexeme (char '(')) (lexeme (char ')')) (sepBy parseValue (lexeme (char ',')))

    case cols of
        Just colList | length colList /= length values ->
            fail "Error: Number of columns does not match number of values."
        _ -> return $ InsertStmt table cols values

parseUpdate :: Parser SQLStatement
parseUpdate = lexeme $ do
    void $ lexeme (stringCI "update")
    table <- identifier
    void $ lexeme (stringCI "set")
    updates <- sepBy parseAssignment (lexeme (char ','))
    cond <- optionMaybe parseWhere
    return $ UpdateStmt table updates cond

parseDrop :: Parser SQLStatement
parseDrop = lexeme $ do
    void $ lexeme (stringCI "drop")
    void $ lexeme (stringCI "table")
    DropStmt <$> identifier

parseCreateDb :: Parser SQLStatement
parseCreateDb = lexeme $ do
    void $ lexeme (stringCI "create")
    void $ lexeme (stringCI "database")
    CreateDbStmt <$> identifier

parseDropDb :: Parser SQLStatement
parseDropDb = lexeme $ do
    void $ lexeme (stringCI "drop")
    void $ lexeme (stringCI "database")
    DropDbStmt <$> identifier

parseDelete :: Parser SQLStatement
parseDelete = lexeme $ do
    void $ lexeme (stringCI "delete")
    void $ lexeme (stringCI "from")
    table <- identifier
    cond <- optionMaybe parseWhere
    return $ DeleteStmt table cond

parseWhere :: Parser Condition
parseWhere = lexeme $ do
    void $ lexeme (stringCI "where")
    parseCondition

parseSQL :: Parser SQLStatement
parseSQL = lexeme $ do
    stmt <- try parseSelect
        <|> try parseCreate
        <|> try parseInsert
        <|> try parseUpdate
        <|> try parseDrop
        <|> try parseCreateDb
        <|> try parseDropDb
        <|> parseDelete
    _ <- lexeme (char ';')
    return stmt