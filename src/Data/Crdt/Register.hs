{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.Crdt.Register where

import Data.Crdt

class Register r val | r -> val where
   set     :: r -> val -> r
   get     :: r -> val
   resolve :: r -> r -> r

instance (Structure r val val) => Register r val where
   set     = update
   get     = value
   resolve = merge

instance (Register r val) => (Structure r val val) where
    value  = get
    update = set
    merge  = resolve
