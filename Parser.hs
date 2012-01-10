{-# LANGUAGE OverloadedStrings #-}
module Parser where

import Text.ParserCombinators.Parsec
import Control.Monad

data JelloVal =
  Symbol String |
  Integer Integer |
  List [JelloVal] deriving (Show, Read)

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
