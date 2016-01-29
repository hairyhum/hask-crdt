{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Crdt.Counter where

import qualified Data.Map as Map

import Data.Crdt

data CounterOp n = Inc n | Dec n

class Counter s val | s -> val where
    inc   :: s -> val -> s
    dec   :: s -> val -> s
    toVal :: s -> val
    union :: s -> s -> s
 
instance (Counter c val) => (Structure c (CounterOp val) val) where
    value = toVal
    update c (Inc n) = inc c n
    update c (Dec n) = dec c n
    merge = union
    pack  = merge

instance (Structure c (CounterOp val) val) => Counter c val where
    toVal   = value
    inc c n = update c (Inc n)
    dec c n = update c (Dec n)
    union   = merge

data ACounter a n = ACounter { actor :: a, values :: (Map.Map a n) } 
    deriving (Show, Eq, Read)

instance (Num n, Ord n, Eq a, Ord a) => Counter (ACounter a n) n where
    toVal (ACounter { values = v }) = Map.foldr (+) (fromInteger 0) v
    inc (ACounter { actor = actor, values = values }) num = 
        ACounter { actor = actor, values = updated_values }
        where
            updated_values = Map.adjust (num +) actor values
    dec ac _ = ac
    union counter1 counter2 = 
        counter1 { values = Map.unionWith (max) v1 v2 }
        where
            v1 = values counter1
            v2 = values counter2

