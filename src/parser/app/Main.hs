{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)

-- Definirea tipurilor de date pentru AST

data SQLStatement
    = SelectStmt [String] String (Maybe Condition)
    | CreateStmt String [(String, String, String)]
    | InsertStmt String [String] [String]
    | UpdateStmt String [(String, String)] (Maybe Condition)
    | DropStmt String
    deriving (Show, Generic, ToJSON)

data Condition
    = Equals String String
    | GreaterThan String String
    | LessThan String String
    | And Condition Condition
    | Or Condition Condition
    | Not Condition
    deriving (Show, Generic, ToJSON)

-- Definirea parserului folosind Parsec

identifier :: Parser String
identifier = many1 (letter <|> digit <|> char '_')

parseSelect :: Parser SQLStatement
parseSelect = do
    void $ string "SELECT "
    cols <- sepBy identifier (string ", ")
    void $ string " FROM "
    table <- identifier
    cond <- optionMaybe parseWhere
    return $ SelectStmt cols table cond

parseCreate :: Parser SQLStatement
parseCreate = do
    void $ string "CREATE TABLE "
    table <- identifier
    cols <- between (char '(') (char ')') (sepBy parseColumn (string ", "))
    return $ CreateStmt table cols

parseColumn :: Parser (String, String, String)
parseColumn = do
    colName <- identifier
    colType <- identifier
    colConstraint <- option "" identifier
    return (colName, colType, colConstraint)

parseInsert :: Parser SQLStatement
parseInsert = do
    void $ string "INSERT INTO "
    table <- identifier
    cols <- between (char '(') (char ')') (sepBy identifier (string ", "))
    void $ string " VALUES "
    values <- between (char '(') (char ')') (sepBy identifier (string ", "))
    return $ InsertStmt table cols values

parseUpdate :: Parser SQLStatement
parseUpdate = do
    void $ string "UPDATE "
    table <- identifier
    void $ string " SET "
    updates <- sepBy ((,) <$> identifier <*> (string " = " *> identifier)) (string ", ")
    cond <- optionMaybe parseWhere
    return $ UpdateStmt table updates cond

parseDrop :: Parser SQLStatement
parseDrop = do
    void $ string "DROP TABLE "
    DropStmt <$> identifier

parseWhere :: Parser Condition
parseWhere = do
    void $ string " WHERE "
    Equals <$> identifier <*> (string " = " *> identifier)

parseSQL :: Parser SQLStatement
parseSQL = do
    stmt <- try parseSelect <|> try parseCreate <|> try parseInsert <|> try parseUpdate <|> parseDrop
    void $ char ';'
    return stmt

main :: IO ()
main = do
    input <- getLine
    case parse (parseSQL <* eof) "SQL statement" input of
        Left err ->
            if last input /= ';' then
                putStrLn "Syntax error: Query must end with ';'"
            else
                putStrLn $ "Syntax error: " ++ show err
        Right ast -> do
            let jsonOutput = encode ast
            B.writeFile "./src/output.json" jsonOutput
