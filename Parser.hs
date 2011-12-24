{-# LANGUAGE OverloadedStrings #-}
module Parser where

import qualified Data.Attoparsec as A
import qualified Data.AttoLisp as L
import Data.Attoparsec.Number (Number (I, D))
import Control.Monad.State.Strict
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Internal as HT

newtype Identifier = Identifier HT.Text deriving (Show, Read)

data JelloTree =
  JelloNote Identifier [JelloTree] |
  JelloFunc Identifier [JelloTree] |
  JelloSymbol HT.Text |
  JelloString HT.Text |
  JelloInteger Integer |
  JelloList [JelloTree] deriving (Show, Read)

tokenize = A.eitherResult . A.parse L.lisp

lispToSyntaxTree :: L.Lisp -> Either String JelloTree
lispToSyntaxTree (L.List ((L.Symbol val):vals))
  | (T.head val) == ':' = do
    rest <- mapM lispToSyntaxTree vals
    return $ JelloNote (Identifier val) rest
  | otherwise = do
    rest <- mapM lispToSyntaxTree vals
    return $ JelloFunc (Identifier val) rest
lispToSyntaxTree (L.Symbol sym) = Right $ JelloSymbol sym
lispToSyntaxTree (L.String str) = Right $ JelloString str
lispToSyntaxTree (L.Number (I int)) = Right $ JelloInteger int
lispToSyntaxTree (L.Number (D _)) = Left "no support for non-integer numbers yet!"
lispToSyntaxTree (L.DotList _ _) = Left "dotted lists aren't used in jello."
