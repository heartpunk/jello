{-# LANGUAGE OverloadedStrings #-}
module Interpreter where

import qualified Data.AttoLisp as L

runLisp :: L.Lisp -> IO ()
runLisp = undefined

let' :: Monad m => identifier -> value -> m ()
let' = undefined
