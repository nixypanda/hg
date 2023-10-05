module Main where

import Control.Monad (unless)
import Data.Char (isDigit)
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String
type ParsingError = ParseErrorBundle String Void

reserved :: [Char]
reserved = ['\\']

gLit :: Parser (Parser Char)
gLit = do
    c <- escaped <|> noneOf reserved
    pure $ char c
  where
    escaped :: Parser Char
    escaped = char '\\' *> oneOf reserved

gDigit :: Parser (Parser Char)
gDigit = do
    _ <- string "\\d"
    pure $ satisfy isDigit

grep :: Parser (Parser Char)
grep = choice [gDigit, gLit]

matchPattern :: String -> String -> Either ParsingError Char
matchPattern pattern input = do
    patternParser <- parse grep "" pattern
    parse patternParser "" input

main :: IO ()
main = do
    args <- getArgs
    let pattern = args !! 1
    input_line <- getLine

    unless (head args == "-E") $ do
        putStrLn "Expected first argument to be '-E'"
        exitFailure
    case matchPattern pattern input_line of
        Right result -> do
            print result
            exitSuccess
        Left err -> do
            print err
            exitFailure

