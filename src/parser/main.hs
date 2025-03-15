import Data.List (isSuffixOf)
data SQLStatement =
      SelectStmt [String] String (Maybe Condition)
    | CreateStmt String [(String, String)]
    | InsertStmt String [String] [String]
    | UpdateStmt String [(String, String)] (Maybe Condition)
    | DropStmt String
    deriving (Show)

data Condition = Equals String String | GreaterThan String String | LessThan String String | And Condition Condition | Or Condition Condition | Not Condition
    deriving (Show)

-- Functie pentru serializare JSON simpla
serializeJSON :: SQLStatement -> String
serializeJSON (SelectStmt cols table cond) =
    "{\"type\": \"SELECT\", \"columns\": " ++ (if cols == ["*"] then "\"*\"" else show cols) ++ ", \"table\": \"" ++ table ++ "\"" ++ maybe "" (", \"where\": " ++) (fmap serializeCond cond) ++ "}"
serializeJSON (CreateStmt table cols) =
    "{\"type\": \"CREATE\", \"table\": \"" ++ table ++ "\", \"columns\": " ++ show cols ++ "}"
serializeJSON (InsertStmt table cols values) =
    "{\"type\": \"INSERT\", \"table\": \"" ++ table ++ "\", \"columns\": " ++ show cols ++ ", \"values\": " ++ show values ++ "}"
serializeJSON (UpdateStmt table updates cond) =
    "{\"type\": \"UPDATE\", \"table\": \"" ++ table ++ "\", \"updates\": " ++ show updates ++ maybe "" (", \"where\": " ++) (fmap serializeCond cond) ++ "}"
serializeJSON (DropStmt table) =
    "{\"type\": \"DROP\", \"table\": \"" ++ table ++ "\"}"

serializeCond :: Condition -> String
serializeCond (Equals col val) = "{\"equals\": [\"" ++ col ++ "\", \"" ++ val ++ "\"]}"
serializeCond (GreaterThan col val) = "{\"greater_than\": [\"" ++ col ++ "\", \"" ++ val ++ "\"]}"
serializeCond (LessThan col val) = "{\"less_than\": [\"" ++ col ++ "\", \"" ++ val ++ "\"]}"
serializeCond (And c1 c2) = "{\"and\": [" ++ serializeCond c1 ++ ", " ++ serializeCond c2 ++ "]}"
serializeCond (Or c1 c2) = "{\"or\": [" ++ serializeCond c1 ++ ", " ++ serializeCond c2 ++ "]}"
serializeCond (Not c) = "{\"not\": " ++ serializeCond c ++ "}"

-- Parser simplu
parseSQL :: String -> Maybe SQLStatement
parseSQL input =
    let trimmedInput = reverse (dropWhile (== ' ') (reverse input)) in
    if not (";" `isSuffixOf` trimmedInput)
        then Nothing
        else case words (init trimmedInput) of
            ("SELECT":"*":"FROM":table:whereClause) ->
                Just (SelectStmt ["*"] table (parseWhere whereClause))
            ("SELECT":cols) -> let (columnList, rest) = break (== "FROM") cols in
                                if "*" `elem` columnList && length columnList > 1
                                    then Nothing
                                    else case rest of
                                        (_:table:whereClause) -> let (whereKey, whereExpr) = break (== "WHERE") whereClause in
                                                                  Just (SelectStmt (map (filter (/= ',')) columnList) table (parseWhere (drop 1 whereExpr)))
                                        _ -> Nothing
            ("CREATE":"TABLE":table:cols) -> Just (CreateStmt table (parseColumns cols))
            ("INSERT":"INTO":table:values) -> let (colPart, valPart) = break (== "VALUES") values in
                                                Just (InsertStmt table colPart (tail valPart))
            ("UPDATE":table:updates) -> let (setPart, wherePart) = break (== "WHERE") updates in
                                         Just (UpdateStmt table (parseSet (unwords setPart)) (parseWhere (tail wherePart)))
            ("DROP":"TABLE":table:_) -> Just (DropStmt table)
            _ -> Nothing

parseColumns :: [String] -> [(String, String)]
parseColumns cols = [(c, "TEXT") | c <- cols]

parseSet :: String -> [(String, String)]
parseSet setStr = [(k, v) | [k, v] <- map words (splitOn ',' setStr)]

parseWhere :: [String] -> Maybe Condition
parseWhere [] = Nothing
parseWhere (col:"=":val:rest) = Just (Equals col val)
parseWhere (col:">":val:rest) = Just (GreaterThan col val)
parseWhere (col:"<":val:rest) = Just (LessThan col val)
parseWhere _ = Nothing

-- Utilitare pentru split
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = [[]]
splitOn delim xs = case break (== delim) xs of
    (before, []) -> [before]
    (before, _:after) -> before : splitOn delim after


-- Main: Citire input si scriere JSON
main :: IO ()
main = do
    query <- getLine
    case parseSQL query of
        Just ast -> do
            let jsonOutput = serializeJSON ast
            writeFile "output.json" jsonOutput
            putStrLn "Query OK"
        Nothing -> do
            writeFile "output.json" ""
            putStrLn "Unknown SQL Statement"
