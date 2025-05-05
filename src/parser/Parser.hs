{-# LANGUAGE OverloadedStrings #-}

module Parser (parseSQL) where

import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)
import Data.Functor (($>))

import AST
import Utils

parseSelect :: Parser SQLStatement
parseSelect = lexeme $ do
    void $ lexeme (string "SELECT")
    cols <- try (lexeme (string "*") $> ["*"]) <|> sepBy identifier (lexeme (char ','))
    void $ lexeme (string "FROM")
    table <- identifier
    cond <- optionMaybe parseWhere
    return $ SelectStmt cols table cond

parseCreate :: Parser SQLStatement
parseCreate = lexeme $ do
    void $ lexeme (string "CREATE")
    void $ lexeme (string "TABLE")
    table <- identifier
    cols <- between (lexeme (char '(')) (lexeme (char ')')) (sepBy parseColumn (lexeme (char ',')))
    return $ CreateStmt table cols

parseInsert :: Parser SQLStatement
parseInsert = lexeme $ do
    void $ lexeme (string "INSERT")
    void $ lexeme (string "INTO")
    table <- identifier
    cols <- optionMaybe $ try (between (lexeme (char '(')) (lexeme (char ')')) (sepBy identifier (lexeme (char ','))))
    void $ lexeme (string "VALUES")
    values <- between (lexeme (char '(')) (lexeme (char ')')) (sepBy parseValue (lexeme (char ',')))

    case cols of
        Just colList | length colList /= length values ->
            fail "Error: Number of columns does not match number of values."
        _ -> return $ InsertStmt table cols  values

parseUpdate :: Parser SQLStatement
parseUpdate = lexeme $ do
    void $ lexeme (string "UPDATE")
    table <- identifier
    void $ lexeme (string "SET")
    updates <- sepBy parseAssignment (lexeme (char ','))
    cond <- optionMaybe parseWhere
    return $ UpdateStmt table updates cond

parseDrop :: Parser SQLStatement
parseDrop = lexeme $ do
    void $ lexeme (string "DROP")
    void $ lexeme (string "TABLE")
    DropStmt <$> identifier

parseCreateDb :: Parser SQLStatement
parseCreateDb = lexeme $ do
    void $ lexeme (string "CREATE")
    void $ lexeme (string "DATABASE")
    CreateDbStmt <$> identifier

parseDropDb :: Parser SQLStatement
parseDropDb = lexeme $ do
    void $ lexeme (string "DROP")
    void $ lexeme (string "DATABASE")
    DropDbStmt <$> identifier

parseDelete :: Parser SQLStatement
parseDelete = lexeme $ do
    void $ lexeme (string "DELETE")
    void $ lexeme (string "FROM")
    table <- identifier
    cond <- optionMaybe parseWhere
    return $ DeleteStmt table cond

parseWhere :: Parser Condition
parseWhere = lexeme $ do
    void $ string "WHERE"
    parseCondition

parseSQL :: Parser SQLStatement
parseSQL = lexeme $ do
    stmt <- try parseSelect <|> try parseCreate <|> try parseInsert <|> try parseUpdate <|> try parseDrop <|> try parseCreateDb <|> try parseDropDb <|> parseDelete
    _ <- lexeme (char ';')
    return stmt