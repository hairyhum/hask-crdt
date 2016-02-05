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

instance (Structure c (CounterOp val) val) => Counter c val where
    toVal   = value
    inc c n = update c (Inc n)
    dec c n = update c (Dec n)
    union   = merge

