
{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}

import Data.Aeson hiding (Value)
import GHC.Generics
import qualified Data.ByteString.Lazy as B
import qualified Data.Map as Map
import System.IO
import Data.Maybe (fromMaybe)
import Control.Applicative

-- Reprezintă o valoare constantă din JSON
data Value = Value
  { valValue :: Int
  , valType :: String
  } deriving (Show, Generic)

instance FromJSON Value where
  parseJSON = withObject "Value" $ \v ->
    Value <$> v .: "value"
          <*> v .: "valueType"

-- Reprezintă condiția din JSON
data Condition
  = CompOp { column :: String, operation :: String, compValue :: Value }
  | LogicOp { left :: Condition, operation :: String, right :: Condition }
  deriving (Show, Generic)

instance FromJSON Condition where
  parseJSON = withObject "Condition" $ \v ->
    -- Încearcă mai întâi LogicOp, apoi CompOp
    (LogicOp <$> v .: "left" <*> v .: "operation" <*> v .: "right")
    <|> (CompOp <$> v .: "column" <*> v .: "operation" <*> v .: "value")

-- Reprezintă întregul query
data Query = Query
  { columns :: [String]
  , condition :: Condition
  , statement :: String
  , table :: String
  } deriving (Show, Generic)

instance FromJSON Query

-- Evaluează expresia condițională
evalCondition :: Condition -> Map.Map String Int -> Bool
evalCondition (CompOp col op (Value val _)) row =
  case Map.lookup col row of
    Just x -> applyOp op x val
    Nothing -> False
evalCondition (LogicOp l op r) row =
  case op of
    "AND" -> evalCondition l row && evalCondition r row
    "OR"  -> evalCondition l row || evalCondition r row
    _     -> False

-- Aplica o operație de comparație între două Int-uri
applyOp :: String -> Int -> Int -> Bool
applyOp "="  = (==)
applyOp "!=" = (/=)
applyOp ">"  = (>)
applyOp "<"  = (<)
applyOp ">=" = (>=)
applyOp "<=" = (<=)
applyOp _    = \_ _ -> False

-- Parsează inputul de la stdin în Map
parseInput :: String -> Map.Map String Int
parseInput s = Map.fromList $ parseWords (words s)
  where
    parseWords (k:v:rest) = (k, read v) : parseWords rest
    parseWords _ = []

-- Main
main :: IO ()
main = do
  jsonData <- B.readFile "output.json"
  let mQuery = decode jsonData :: Maybe Query
  case mQuery of
    Nothing -> putStrLn "Invalid JSON."
    Just query -> do
      input <- getLine
      let row = parseInput input
          result = evalCondition (condition query) row
      putStrLn $ if result then "True" else "False"
