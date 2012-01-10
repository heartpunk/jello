{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Text.ParserCombinators.Parsec
import Control.Monad
import Data.List

data JelloVal =
  Func ([JelloVal] -> JelloVal) |
  Symbol String |
  Integer Integer |
  List [JelloVal]

instance Show JelloVal where
    show (Func _) = "(* -> *)"
    show (Symbol str) = "Symbol " ++ str
    show (Integer int) = "Integer " ++ show int
    show (List xs) = "[" ++ (intercalate ", " $ map show xs) ++ "]"

symbol = do
    first <- letter <|> punc
    rest <- many $ letter <|> punc <|> digit
    return $ Symbol $ first : rest

int = liftM (Integer . read) $ many1 digit

list = do
    char '('
    spaces
    vals <- parseJello `sepBy` spaces
    spaces
    char ')'
    return $ List vals

parseJello :: Parser JelloVal
parseJello = symbol
         <|> int
         <|> list

-- really only for debugging.  and puns.
airyParse :: String -> JelloVal
airyParse input =
    case parse parseJello "jello" input of
        Right val -> val
        Left msg -> error $ show msg

punc = oneOf "*+-/"
