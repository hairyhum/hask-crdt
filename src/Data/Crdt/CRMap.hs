{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.Crdt.CRMap where

import Data.Crdt
import qualified Data.Map as Map

data MapOp k v = Set k v | Unset k


class (Ord key, Eq key) => CRMap s key val | s -> key val where
    toMap :: s -> Map.Map key val
    get   :: s -> key -> Maybe val
    set   :: s -> key -> val -> s
    unset :: s -> key -> s
    union :: s -> s -> s
    

instance (Ord k, Eq k, Structure m (MapOp k v) (Map.Map k v)) => CRMap m k v where
    get m k   = Map.lookup k (toMap m)
    set m k v = update m (Set k v) 
    unset m k = update m (Unset k)
    union     = merge
    toMap     = value

instance (CRMap m k v) => Structure m (MapOp k v) (Map.Map k v) where
    update m (Set k v) = set m k v
    update m (Unset k) = unset m k 
    value = toMap
    merge = union
