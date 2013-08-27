module Monad where

import Control.Monad.State
import Control.Monad.Error

data Tree = Leaf Int
          | Bin Tree Tree

vals :: Tree -> [Int]
vals t = snd $ runState (vals' t) []
    where
        vals' :: Tree -> State [Int] ()
        vals' (Leaf val) 
            = do
                acc <- get 
                put (val : acc)
        vals' (Bin l r)
            = do
                acc <- get
                vals' r
                acc' <- get
                vals' l
                acc'' <- get
                put acc''

height :: Tree -> (Int, Int)
height t = case runState (runErrorT $ htAdp' t) 0 of
            (Left str, ht) -> error str
            (Right dt, ht) -> (dt, ht)
    where
        htAdp' :: Tree -> ErrorT String (State Int) Int
        htAdp' (Leaf val)
            = do
                depth <- lift get
                if val > 7 
                    then throwError "You supplied a value greater then 7" 
                    else return depth
        htAdp' (Bin l r)
            = do
                depth <- lift get
                lift $ put (depth + 1)
                rht <- htAdp' r `catchError` (const $ return 0)
                lht <- htAdp' l `catchError` (const $ return 0)
                return $ max lht rht
