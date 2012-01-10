{-# LANGUAGE OverloadedStrings #-}
module Interpreter where

import Parser-- (JelloVal (Symbol, Integer, List))

eval :: JelloVal -> JelloVal
eval (List ((Symbol func):args)) =
    case lookup func builtins of
        Nothing -> error $ (show func) ++ " is undefined."
        Just f -> f $ map eval args
eval val@(Integer _) = val
eval val@(Symbol _) = error "no use for these outside of function names yet."
    
numericBinOp :: (Integer -> Integer -> Integer) -> [JelloVal] -> JelloVal
numericBinOp op = Integer . foldl1 op . map fromInteger
  where fromInteger (Integer i) = i
        fromInteger foo = error $ (show foo) ++ " is not a number."

builtins :: [(String, ([JelloVal] -> JelloVal))]
builtins =
    [("+", numericBinOp (+)),
     ("-", numericBinOp (-)),
     ("/", numericBinOp (div)),
     ("*", numericBinOp (*))]

runJello = eval . airyParse
