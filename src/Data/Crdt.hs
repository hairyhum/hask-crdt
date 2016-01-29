{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Crdt (Structure(value, update, merge, pack)) where

import qualified Data.Map as Map

class Structure s op val | s -> op val where 
    value :: s -> val
    update :: s -> op -> s
    merge :: s -> s -> s
    pack :: s -> s -> s





