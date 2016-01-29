{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Crdt.PNCounter where

import Data.Crdt
import qualified Data.Crdt.Counter as Counter

import qualified Data.Map as Map

data PNCounter a n = PNCounter { actor :: a,  incs :: (Map.Map a n),  decs :: (Map.Map a n)} 
    deriving (Show, Eq, Read)

instance (Num n, Ord n, Eq a, Ord a) => Counter.Counter (PNCounter a n) n where
    toVal (PNCounter { incs = incs, decs = decs }) = 
        pos - neg
        where
            pos = Map.foldr (+) (fromInteger 0) incs
            neg = Map.foldr (+) (fromInteger 0) decs
    inc counter num = 
        counter { incs = updated_values }
        where
            updated_values = Map.adjust (num +) (actor counter) (incs counter)
    dec counter num = 
        counter { decs = updated_values }
        where
            updated_values = Map.adjust (num +) (actor counter) (decs counter)
    union counter1 counter2 = 
        counter1 { 
            incs = Map.unionWith (max) i1 i2, 
            decs = Map.unionWith (max) d1 d2 }
        where
            i1 = incs counter1
            i2 = incs counter2
            d1 = decs counter1
            d2 = decs counter2

    
