{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec (parse, eof)
import Data.Aeson (encode, Value(Null))

import Parser (parseSQL)

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
