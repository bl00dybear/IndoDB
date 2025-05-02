{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module AST where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), object, (.=), Value)

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
    deriving (Show, Generic)

instance ToJSON ConstraintType where
    toJSON NotNull = "NotNull"
    toJSON PrimaryKey = "PrimaryKey"
    toJSON ForeignKey = "ForeignKey"

data DataType
    = IntType
    | FloatType
    | VarCharType Int
    | DateType
    deriving (Show, Generic)

instance ToJSON DataType where
    toJSON IntType = "Int"
    toJSON FloatType = "Float"
    toJSON (VarCharType n) = object ["type" .= ("String" :: String), "length" .= n]
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
    = Equals String SQLValue
    | NotEquals String SQLValue
    | GreaterThan String SQLValue
    | LessThan String SQLValue
    | GreaterOrEqual String SQLValue
    | LessOrEqual String SQLValue
    | And Condition Condition
    | Or Condition Condition
    deriving (Show, Generic)

instance ToJSON Condition where
    toJSON (Equals col val) = object ["operation" .= ("=" :: String), "column" .= col, "value" .= val]
    toJSON (NotEquals col val) = object ["operation" .= ("!=" :: String), "column" .= col, "value" .= val]
    toJSON (GreaterThan col val) = object ["operation" .= (">" :: String), "column" .= col, "value" .= val]
    toJSON (LessThan col val) = object ["operation" .= ("<" :: String), "column" .= col, "value" .= val]
    toJSON (GreaterOrEqual col val) = object ["operation" .= (">=" :: String), "column" .= col, "value" .= val]
    toJSON (LessOrEqual col val) = object ["operation" .= ("<=" :: String), "column" .= col, "value" .= val]
    toJSON (And c1 c2) = object ["operation" .= ("AND" :: String), "left" .= c1, "right" .= c2]
    toJSON (Or c1 c2) = object ["operation" .= ("OR" :: String), "left" .= c1, "right" .= c2]
