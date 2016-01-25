{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Crdt (Structure(get, update, merge, pack), Counter, CounterOp(Inc, Dec), Actor, ACounter) where

import qualified Data.Map as Map

class Structure s op val | s -> op val where 
    get :: s -> val
    update :: s -> op -> s
    merge :: s -> s -> s
    pack :: s -> s -> s

data CounterOp n = Inc n | Dec n

class (Structure c (CounterOp val) val) => Counter c val where
    inc :: c -> val -> c
    dec :: c -> val -> c

type Actor = Integer
data ACounter n = ACounter { actor :: Actor, values :: (Map.Map Actor n) } 
    deriving (Show, Eq, Read)

instance (Num n, Ord n) => Structure (ACounter n) (CounterOp n) n where
    get (ACounter { values = v }) = 
        Map.foldr (+) (fromInteger 0) v
    update (ACounter { actor = actor, values = values }) (Inc num) = 
        ACounter { actor = actor, values = updated_values }
        where
            updated_values = Map.adjust (num +) actor values
    merge counter1 counter2 = 
        counter1 { values = Map.unionWith (max) v1 v2 }
        where
            v1 = values counter1
            v2 = values counter2
    pack = merge

instance (Num n, Ord n) => Counter (ACounter n) n where
    inc c n = update c (Inc n)
    dec c n = update c (Dec n)





