{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as B
import Text.Parsec (parse, eof)
import Text.Parsec.Error (errorMessages, Message(..))
import Data.Aeson (encode, object, (.=))

import Parser (parseSQL)

main :: IO ()
main = do
    input <- getLine
    case parse (parseSQL <* eof) "" input of
        Left err -> do
            let msgs   = errorMessages err
                custom = [s | Message s <- msgs]
                errMsg = if not (null custom)
                         then head custom
                         else "Invalid SQL Statement"
                jsonErr = object ["error" .= errMsg]
            B.writeFile "../output/output.json" (encode jsonErr)
        Right ast -> do
            -- On success, encode the AST
            let jsonOutput = encode ast
            B.writeFile "../output/output.json" jsonOutput
