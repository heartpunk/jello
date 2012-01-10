{-# LANGUAGE OverloadedStrings #-}
module Interpreter where

import Parser-- (JelloVal (Symbol, Integer, List))
import Control.Monad.State

eval :: JelloVal -> State [(String, JelloVal)] JelloVal
eval (List ((Symbol "let"):(Symbol name):value:body)) = do
    modify ((name, value):)
    fmap last $ sequence $ map eval body
--lambda introduces the issue of nested scopes, so i'm leaving them out for this iteration.
--eval (List ((Symbol "lambda"):argNames:body)) =
eval (List ((Symbol func):args)) = do
    s <- get
    vals <- mapM eval args
    return $ (lookup' func s) $ vals
eval val@(List []) = return val
eval val@(Integer _) = return val
eval (Symbol name) = do
    s <- get
    return $ (lookup'' name s)

lookup' :: (Show a, Eq a) => a -> [(a, JelloVal)] -> ([JelloVal] -> JelloVal)
lookup' name state = maybe (error $ (show name) ++ " is undefined.") fromFunc $ lookup name state

lookup'' :: (Show a, Eq a) => a -> [(a, JelloVal)] -> JelloVal
lookup'' name state = maybe (error $ (show name) ++ " is undefined.") id $ lookup name state

fromFunc (Func f) = f
fromSym (Symbol s) = s

numericBinOp :: (Integer -> Integer -> Integer) -> [JelloVal] -> JelloVal
numericBinOp op = Integer . foldl1 op . map fromInteger
  where fromInteger (Integer i) = i
        fromInteger foo = error $ (show foo) ++ " is not a number."

builtins :: [(String, JelloVal)]
builtins = map (\(a,b) -> (a, Func b))
    [("+", numericBinOp (+)),
     ("-", numericBinOp (-)),
     ("/", numericBinOp (div)),
     ("*", numericBinOp (*))]

runJello = (\expr -> runState (eval expr) builtins) . airyParse
