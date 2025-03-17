{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)
import Data.Functor ((<$>), ($>))

data ConstraintType = NotNull | PrimaryKey | ForeignKey
    deriving (Show, Generic, ToJSON)

data SQLStatement
    = SelectStmt [String] String (Maybe Condition)
    | CreateStmt String [(String, String, Maybe ConstraintType)]
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
