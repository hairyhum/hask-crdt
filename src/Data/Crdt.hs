{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Crdt (Structure(value, update, merge), Packable(pack)) where

import qualified Data.Map as Map

class Structure s op val | s -> op val where 
    value :: s -> val
    update :: s -> op -> s
    merge :: s -> s -> s

class Packable s where
    pack :: s -> s -> s
-- PACK PRECONDITION: s1 > s2
    pack = const
-- PACK RULE: value (merge (pack a b) b) = value (merge a b)




