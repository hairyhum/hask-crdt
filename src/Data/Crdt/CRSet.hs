{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.Crdt.CRSet where 

import Data.Crdt
import qualified Data.Set as Set

data SetOp n = Add n | Remove n

class (Ord val, Eq val) => CRSet s val | s -> val where
    toSet  :: s -> Set.Set val
    add    :: s -> val -> s
    remove :: s -> val -> s
    union  :: s -> s -> s

instance (CRSet s val) => Structure s (SetOp val) (Set.Set val) where
    value               = toSet
    update s (Add v)    = add s v
    update s (Remove v) = remove s v
    merge               = union

instance (Ord val, Eq val, Structure s (SetOp val) (Set.Set val)) => CRSet s val where
    toSet        = value
    add s val    = update s (Add val)
    remove s val = update s (Remove val)
    union        = merge
