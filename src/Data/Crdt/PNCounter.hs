{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Crdt.PNCounter where

import Data.Crdt
import Data.Crdt.Counter

import qualified Data.Map as Map

data PNCounter a n = PNCounter { actor :: a,  incs :: (Map.Map a n),  decs :: (Map.Map a n)} 
    deriving (Show, Eq, Read)

instance {-# OVERLAPPING #-} (Num n, Ord n, Eq a, Ord a) => Counter (PNCounter a n) n where
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


instance (Num n, Ord n, Eq a, Ord a) => Packable (PNCounter a n) where
    pack c1 c2 =
        res_counter { incs = (Map.differenceWith not_equal ires i2),
                      decs = (Map.differenceWith not_equal dres d2) }
        where
            ires = incs res_counter
            i2 = incs c2 
            dres = decs res_counter
            d2 = decs c2 
            
            not_equal a b = if a == b then Nothing else Just a
            res_counter = if diff > 0 then inc c2 diff else dec c2 (negate diff)
            diff = (toVal c1) - (toVal c2)
        
