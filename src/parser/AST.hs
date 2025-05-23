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
    | SQLBool Bool
    | SQLNull
    deriving (Show, Generic)

instance ToJSON SQLValue where
    toJSON (SQLString s) = object ["valueType" .= ("String" :: String), "value" .= s]
    toJSON (SQLInt i)    = object ["valueType" .= ("Int" :: String), "value" .= i]
    toJSON (SQLFloat f)  = object ["valueType" .= ("Float" :: String), "value" .= f]
    toJSON (SQLDate d)   = object ["valueType" .= ("Date" :: String), "value" .= d]
    toJSON (SQLBool b)   = object ["valueType" .= ("Bool" :: String), "value" .= b]
    toJSON SQLNull       = object ["valueType" .= ("Null" :: String), "value" .= ("NULL" :: String)]

data ConstraintType = NotNull | Unique | PrimaryKey | ForeignKey
    deriving (Show, Generic)

instance ToJSON ConstraintType where
    toJSON NotNull = "NotNull"
    toJSON Unique = "Unique"
    toJSON PrimaryKey = "PrimaryKey"
    toJSON ForeignKey = "ForeignKey"

data DataType
    = IntType
    | FloatType
    | VarCharType Int
    | DateType
    | BoolType
    deriving (Show, Generic)

instance ToJSON DataType where
    toJSON IntType = "Int"
    toJSON FloatType = "Float"
    toJSON (VarCharType n) = object ["type" .= ("String" :: String), "length" .= n]
    toJSON DateType = "Date"
    toJSON BoolType = "Bool"

data SQLStatement
    = CreateStmt String [(String, DataType, Maybe ConstraintType)]
    | InsertStmt String (Maybe [String]) [SQLValue]
    | DropStmt String
    | SelectStmt [String] String (Maybe Condition)
    | UpdateStmt String [String] [SQLValue] (Maybe Condition)
    | DeleteStmt String (Maybe Condition)
    | CreateDbStmt String
    | DropDbStmt String
    | UseDbStmt String
    | ShowDbStmt
    | ShowTbStmt
    | DescTbStmt String
    deriving (Show, Generic)

instance ToJSON SQLStatement where
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
    toJSON (DropStmt table) = object
        [ "statement" .= ("DropStmt" :: String)
        , "table" .= table
        ]
    toJSON (SelectStmt cols table cond) = object
        [ "statement" .= ("SelectStmt" :: String)
        , "table" .= table
        , "columns" .= cols
        , "condition" .= cond
        ]
    toJSON (UpdateStmt table cols vals cond) = object
        [ "statement" .= ("UpdateStmt" :: String)
        , "table" .= table
        , "columns" .= cols
        , "values" .= vals
        , "condition" .= cond
        ]
    toJSON (DeleteStmt table cond) = object
        [ "statement" .= ("DeleteStmt" :: String)
        , "table" .= table
        , "condition" .= cond
        ]
    toJSON (CreateDbStmt name) = object
        [  "statement" .=("CreateDbStmt" :: String)
        ,  "database" .= name
        ]
    toJSON (DropDbStmt name) = object
        [  "statement" .=("DropDbStmt" :: String)
        ,  "database" .= name
        ]
    toJSON (UseDbStmt name) = object
        [ "statement" .=("UseDbStmt" :: String)
        ,  "database" .= name
        ]
    toJSON ShowDbStmt = object
        [ "statement" .=("ShowDbStmt" :: String)
        ]
    toJSON ShowTbStmt = object
        [ "statement" .=("ShowTbStmt" :: String)
        ]
    toJSON (DescTbStmt name) = object
        [ "statement" .=("DescTbStmt" :: String)
        ,  "table" .= name
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
