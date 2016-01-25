{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module PNCounter where

import Crdt
import qualified Data.Map as Map

data PNCounter n = PNCounter { actor :: Actor,  incs :: (Map.Map Actor n),  decs :: (Map.Map Actor n)} 
    deriving (Show, Eq, Read)

instance (Num n, Ord n) => Structure (PNCounter n) CounterOp n where
    get (PNCounter { incs = incs, decs = decs }) = 
        pos - neg
        where
            pos = Map.foldr (+) (fromInteger 0) incs
            neg = Map.foldr (+) (fromInteger 0) decs
    update counter Inc num = 
        counter { incs = updated_values }
        where
            updated_values = Map.adjust (num +) (actor counter) (incs counter)
    update counter Dec num = 
        counter { decs = updated_values }
        where
            updated_values = Map.adjust (num +) (actor counter) (decs counter)
    merge counter1 counter2 = 
        counter1 { 
            incs = Map.unionWith (max) i1 i2, 
            decs = Map.unionWith (max) d1 d2 }
        where
            i1 = incs counter1
            i2 = incs counter2
            d1 = decs counter1
            d2 = decs counter2
    pack = pack

    
