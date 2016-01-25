{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module TPSet where

import Crdt
import GSet

import qualified Data.Set as Set

data TPSet a = TPSet { adds :: GSet a, removes :: GSet a }

instance (Ord a) => Structure (TPSet a) (SetOp a) (Set.Set a) where
    get s = Set.difference (adds s) (removes s)
    update s (Add el) =
        s{ adds = Set.insert el (adds s)}
    update s (Remove el) =
        if Set.member el (adds s)
            then s{ removes = Set.insert el (removes s)}
            else s
    merge s1 s2 = 
        TPSet { adds = merge adds1 adds2, removes = merge removes1 removes2 }
        where
            adds1 = adds s1
            adds2 = adds s2
            removes1 = removes s1
            removes2 = removes s2
    pack = merge