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
import Data.Data (Data)

-- data definition:

data SQLValue
    = SQLString String
    | SQLInt Int
    | SQLFloat Float
    | SQLDate String
    deriving (Show, Generic, ToJSON)

data ConstraintType = NotNull | PrimaryKey | ForeignKey
    deriving (Show, Generic, ToJSON)

data DataType = Int | Float | VarChar | Date
    deriving (Show, Generic, ToJSON)

data SQLStatement
    = SelectStmt [String] String (Maybe Condition)
    | CreateStmt String [(String, DataType, Maybe ConstraintType)]
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

-- utils:

identifier :: Parser String
identifier = many1 (letter <|> digit <|> char '_')

parseValue :: Parser SQLValue
parseValue = try parseString
         <|> try parseFloat
         <|> try parseInt
         <|> try parseDate

parseString :: Parser SQLValue
parseString = do
    char '\''
    str <- manyTill anyChar (char '\'')
    return $ SQLString str

parseInt :: Parser SQLValue
parseInt = SQLInt . read <$> many1 digit

parseFloat :: Parser SQLValue
parseFloat = do
    num <- many1 digit
    char '.'
    frac <- many1 digit
    return $ SQLFloat (read (num ++ "." ++ frac))

parseDate :: Parser SQLValue
parseDate = do
    char '\''
    year <- count 4 digit
    char '-'
    month <- count 2 digit
    char '-'
    day <- count 2 digit
    char '\''
    return $ SQLDate (year ++ "-" ++ month ++ "-" ++ day)

parseConstraint :: Parser ConstraintType
parseConstraint = choice
    [ try (string "PRIMARY KEY" $> PrimaryKey)
    , try (string "FOREIGN KEY" $> ForeignKey)
    , try (string "NOT NULL" $> NotNull)
    ]

parseDataType :: Parser DataType
parseDataType = choice
    [ try (string "INT" $> Int)
    , try (string "FLOAT" $> Float)
    , try (string "VARCHAR" $> VarChar)
    , try (string "DATE" $> Date)
    ]

parseColumn :: Parser (String, DataType, Maybe ConstraintType)
parseColumn = do
    colName <- identifier
    spaces
    colType <- parseDataType
    spaces
    colConstraint <- optionMaybe (try (spaces *> parseConstraint))
    return (colName, colType, colConstraint)

parseAssignment :: Parser (String, String)
parseAssignment = do
    colName <- identifier
    spaces
    void $ char '='
    spaces
    value <- parseValue
    return (colName, show value)

-- parse statements:

parseSelect :: Parser SQLStatement
parseSelect = do
    void $ string "SELECT "
    cols <- try (string "*" $> ["*"]) <|> sepBy identifier (string ", ")
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
    values <- between (char '(' *> spaces) (spaces *> char ')') (sepBy parseValue (spaces *> char ',' *> spaces))

    case cols of
        Just colList | length colList /= length values ->
            fail "Syntax error: Number of columns does not match number of values."
        _ -> return $ InsertStmt table cols (map show values)

parseUpdate :: Parser SQLStatement
parseUpdate = do
    void $ string "UPDATE "
    table <- identifier
    spaces
    void $ string "SET"
    spaces
    updates <- sepBy parseAssignment (spaces *> char ',' *> spaces)
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

-- main:

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
