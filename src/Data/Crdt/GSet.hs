{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Crdt.GSet where

import Data.Crdt
import qualified Data.Set as Set

type GSet = Set.Set

data SetOp a = Add a | Remove a deriving (Show, Eq, Read)

instance (Ord n) => Structure (GSet n) (SetOp n) (Set.Set n) where
    value s = s
    update s (Add n) =
        Set.insert n s
    merge = Set.union
    pack = merge