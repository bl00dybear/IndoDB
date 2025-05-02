{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
import GHC.Generics (Generic)
import Data.Aeson (ToJSON (toJSON), encode, object, (.=), Value(Null))
import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec
import Text.Parsec.String
import Control.Monad (void)
import Data.Functor (($>))

-- data definition:
data SQLValue
    = SQLString String
    | SQLInt Int
    | SQLFloat Float
    | SQLDate String
    deriving (Show, Generic)

instance ToJSON SQLValue where
    toJSON (SQLString s) = object ["valueType" .= ("String" :: String), "value" .= s]
    toJSON (SQLInt i) = object ["valueType" .= ("Int" :: String), "value" .= i]
    toJSON (SQLFloat f) = object ["valueType" .= ("Float" :: String), "value" .= f]
    toJSON (SQLDate d) = object ["valueType" .= ("Date" :: String), "value" .= d]

data ConstraintType = NotNull | PrimaryKey | ForeignKey
    deriving (Show, Generic, ToJSON)

data DataType = IntType | FloatType | VarCharType | DateType
    deriving (Show, Generic)

instance ToJSON DataType where
    toJSON IntType = "Int"
    toJSON FloatType = "Float"
    toJSON VarCharType = "VarChar"
    toJSON DateType = "Date"

data SQLStatement
    = SelectStmt [String] String (Maybe Condition)
    | CreateStmt String [(String, DataType, Maybe ConstraintType)]
    | InsertStmt String (Maybe [String]) [SQLValue]
    | UpdateStmt String [(String, String)] (Maybe Condition)
    | DropStmt String
    | CreateDbStmt String
    | DropDbStmt String
    | DeleteStmt String (Maybe Condition)
    deriving (Show, Generic)

instance ToJSON SQLStatement where
    toJSON (SelectStmt cols table cond) = object
        [ "statement" .= ("SelectStmt" :: String)
        , "table" .= table
        , "columns" .= cols
        , "condition" .= cond
        ]
    toJSON (CreateStmt table cols) = object
        [ "statement" .= ("CreateStmt" :: String)
        , "table" .= table
        , "columns" .= map (\(n, t, c) -> object ["name" .= n, "type" .= t, "constraint" .= c]) cols
        ]
    toJSON (InsertStmt table cols values) = object
        [ "statement" .= ("InsertStmt" :: String)
        , "table" .= table
        , "columns" .= cols
        , "values" .= values
        ]
    toJSON (UpdateStmt table updates cond) = object
        [ "statement" .= ("UpdateStmt" :: String)
        , "table" .= table
        , "updates" .= map (\(col, val) -> object ["column" .= col, "value" .= val]) updates
        , "condition" .= cond
        ]
    toJSON (DropStmt table) = object
        [ "statement" .= ("DropStmt" :: String)
        , "table" .= table
        ]
    toJSON (CreateDbStmt name) = object
        [  "statement" .=("CreateDbStmt" :: String)
        ,  "database" .= name
        ]
    toJSON (DropDbStmt name) = object
        [  "statement" .=("DropDbStmt" :: String)
        ,  "database" .= name
        ]
    toJSON (DeleteStmt table cond) = object
        [ "statement" .= ("DeleteStmt" :: String)
        , "table" .= table
        , "condition" .= cond
        ]

data Condition
    = Equals String String
    | GreaterThan String String
    | LessThan String String
    | And Condition Condition
    | Or Condition Condition
    | Not Condition
    deriving (Show, Generic)

instance ToJSON Condition where
    toJSON (Equals col val) = object ["type" .= ("Equals" :: String), "column" .= col, "value" .= val]
    toJSON (GreaterThan col val) = object ["type" .= ("GreaterThan" :: String), "column" .= col, "value" .= val]
    toJSON (LessThan col val) = object ["type" .= ("LessThan" :: String), "column" .= col, "value" .= val]
    toJSON (And c1 c2) = object ["type" .= ("And" :: String), "left" .= c1, "right" .= c2]
    toJSON (Or c1 c2) = object ["type" .= ("Or" :: String), "left" .= c1, "right" .= c2]
    toJSON (Not c) = object ["type" .= ("Not" :: String), "condition" .= c]

-- utils:

lexeme :: Parser a -> Parser a
lexeme p = spaces *> p <* spaces

identifier :: Parser String
identifier = lexeme (many1 (letter <|> digit <|> char '_'))

parseValue :: Parser SQLValue
parseValue = lexeme (try parseDate
         <|> try parseFloat
         <|> try parseInt
         <|> try parseString)

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

parseConstraint :: Parser ConstraintType
parseConstraint = lexeme $ choice
    [ try (string "PRIMARY KEY" $> PrimaryKey)
    , try (string "FOREIGN KEY" $> ForeignKey)
    , try (string "NOT NULL" $> NotNull)
    ]

parseDataType :: Parser DataType
parseDataType = lexeme $ choice
    [ try (string "INT" $> IntType)
    , try (string "FLOAT" $> FloatType)
    , try (string "VARCHAR" $> VarCharType)
    , try (string "DATE" $> DateType)
    ]

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
parseCondition = lexeme parseOr

parseOr :: Parser Condition
parseOr = lexeme $ do
    left <- parseAnd
    rest <- optionMaybe (lexeme (string "OR") *> parseOr)
    return $ maybe left (Or left) rest

parseAnd :: Parser Condition
parseAnd = lexeme $ do
    left <- parseNot
    rest <- optionMaybe (lexeme (string "AND") *> parseAnd)
    return $ maybe left (And left) rest

parseNot :: Parser Condition
parseNot = lexeme $
    lexeme (string "NOT") *> (Not <$> parseNot) <|> parseBaseCondition

parseBaseCondition :: Parser Condition
parseBaseCondition = lexeme (parseParens <|> parseComparison)

parseParens :: Parser Condition
parseParens = lexeme $ between (lexeme (char '(')) (lexeme (char ')')) parseCondition

parseComparison :: Parser Condition
parseComparison = lexeme $ do
    col <- identifier
    op <- lexeme $ choice $ map string [">=", "<=", ">", "<", "="]
    val <- parseValue
    return $ case op of
        "="  -> Equals col (show val)
        ">"  -> GreaterThan col (show val)
        "<"  -> LessThan col (show val)
        ">=" -> Or (GreaterThan col (show val)) (Equals col (show val))
        "<=" -> Or (LessThan col (show val)) (Equals col (show val))
        _    -> error "Unexpected comparison operator"

-- parse statements:

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
            fail "Syntax error: Number of columns does not match number of values."
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

-- main:
main :: IO ()
main = do
    input <- getLine
    case parse (parseSQL <* eof) "" input of
        Left err -> do
            print err
            -- comm
            B.writeFile "./output.json" (encode Null)
        Right ast -> do
            let jsonOutput = encode ast
            B.writeFile "./output.json" jsonOutput
