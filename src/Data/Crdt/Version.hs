{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Crdt.Version where

class (Eq v) => Version v actor | v -> actor where
    init     :: v
    ancestor :: v -> v -> Bool
    child    :: v -> v -> Bool
    sibling  :: v -> v -> Bool
    join     :: v -> v -> v
    increase :: actor -> v -> v
    dot      :: actor -> v -> v
    min_diff :: v -> v -> v

    child a b    = ancestor b a
    ancestor a b = child b a
    sibling a b  = not $ (ancestor a b) || (child a b)


