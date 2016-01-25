{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module GSet where

import Crdt
import qualified Data.Set as Set

type GSet = Set.Set

data SetOp a = Add a | Remove a deriving (Show, Eq, Read)

instance (Ord n) => Structure (GSet n) (SetOp n) (Set.Set n) where
    get s = s
    update s (Add n) =
        Set.insert n s
    merge = Set.union
    pack = merge