{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Crdt.ACounter where

import Data.Crdt
import Data.Crdt.Counter

import Control.Applicative ((<|>))
import qualified Data.Map as Map

data ACounter a n = ACounter { actor :: a, values :: (Map.Map a n) } 
    deriving (Show, Eq, Read)

instance {-# OVERLAPPING #-} (Num n, Ord n, Eq a, Ord a) => Counter (ACounter a n) n where
    toVal (ACounter { values = v }) = Map.foldr (+) (fromInteger 0) v
    inc (ACounter { actor = actor, values = values }) num = 
        ACounter { actor = actor, values = updated_values }
        where
            updated_values = Map.alter init_counter actor values
            init_counter x = (fmap (num+) x) <|> (Just num)
    dec ac _ = ac
    union counter1 counter2 = 
        counter1 { values = Map.unionWith (max) v1 v2 }
        where
            v1 = values counter1
            v2 = values counter2

instance (Num n, Ord n, Eq a, Ord a) => Packable (ACounter a n) where
    pack c1 c2 =
        c1 { values = (Map.differenceWith not_equal v1 v2) }
        where
            v1 = values c1
            v2 = values c2 
            not_equal a b = if a == b then Nothing else Just a
