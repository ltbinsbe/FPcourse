module Monad where

import Control.Monad.State

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

htAdp :: Tree -> (Int, Int)
htAdp t = runState (htAdp' t) 0
    where
        htAdp' :: Tree -> State Int Int
        htAdp' (Leaf val)
            = do
                depth <- get
                return depth
        htAdp' (Bin l r)
            = do
                depth <- get
                put (depth + 1)
                rht <- htAdp' r
                lht <- htAdp' l
                return $ max lht rht
