{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Crdt.IntVersion where

import Data.Crdt.Version

instance (Real n) => Version n () where
    init         = 1
    ancestor     = (<)
    increase _   = (1 +)
    dot _        = id
    join         = max
    min_diff a b = 1 + $ min a b
