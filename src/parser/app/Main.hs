{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, encode, Value(Null))
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Data.List (isInfixOf)
import Text.Parsec.String
import Control.Monad (void)
import Data.Functor ((<$>), ($>))

data ConstraintType = NotNull | PrimaryKey | ForeignKey
    deriving (Show, Generic, ToJSON)

data SQLStatement
    = SelectStmt [String] String (Maybe Condition)
    | CreateStmt String [(String, String, Maybe ConstraintType)]
    | InsertStmt String (Maybe [String]) [String]
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

parseConstraint :: Parser ConstraintType
parseConstraint = choice
    [ try (string "PRIMARY KEY" $> PrimaryKey)
    , try (string "FOREIGN KEY" $> ForeignKey)
    , string "NOT NULL" $> NotNull
    ]

parseColumn :: Parser (String, String, Maybe ConstraintType)
parseColumn = do
    colName <- identifier
    spaces
    colType <- identifier
    spaces
    colConstraint <- optionMaybe (try (spaces *> parseConstraint))
    return (colName, colType, colConstraint)

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
    spaces
    cols <- between (char '(' *> spaces) (spaces *> char ')') (sepBy parseColumn (spaces *> char ',' *> spaces))
    return $ CreateStmt table cols

parseInsert :: Parser SQLStatement
parseInsert = do
    void $ string "INSERT INTO "
    spaces
    table <- identifier
    spaces
    cols <- optionMaybe $ try (between (char '(' *> spaces) (spaces *> char ')') (sepBy identifier (spaces *> char ',' *> spaces)))
    spaces
    void $ string "VALUES"
    spaces
    values <- between (char '(' *> spaces) (spaces *> char ')') (sepBy identifier (spaces *> char ',' *> spaces))

    case cols of
        Just colList | length colList /= length values ->
            fail "Syntax error: Number of columns does not match number of values."
        _ -> return $ InsertStmt table cols values

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
    spaces
    _ <- char ';'
    return stmt

main :: IO ()
main = do
    input <- getLine
    case parse (parseSQL <* eof) "" input of
        Left err -> do
            print err
            B.writeFile "./src/output.json" (encode Null)
        Right ast -> do
            let jsonOutput = encode ast
            B.writeFile "./src/output.json" jsonOutput
