{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}


module Data.Crdt.Register where

import Data.Crdt

class Register r val | r -> val where
   set   :: r -> val -> r
   get   :: r -> val
   union :: r -> r -> r
   diff  :: r -> r -> r

instance (Structure r val val) => Register r val where
   set = update
   get = value
   union = merge
   diff  = pack

instance (Register r val) => (Structure r val val) where
    value  = get
    update = set
    merge  = union
    pack   = diff
