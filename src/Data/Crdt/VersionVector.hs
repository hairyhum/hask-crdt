{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Crdt.VersionVector where

import Data.Crdt.Version
import Data.Maybe
import Control.Exception.Base
import qualified Data.Map as Map

type VV actor = Map.Map actor Int

instance (Ord actor) => Version (VV actor) actor where
    init           = Map.empty
    ancestor m1 m2 = Map.null $ Map.difference m1 m2
    increase actor = Map.insertWith (+) actor 1
    dot actor m    = Map.singleton actor $ fromMaybe 1 $ Map.lookup actor m
    join           = Map.unionWith max
    min_diff m1 m2 = Map.mergeWithKey (\_ _ -> Just . (1 +)) 
                                      (Map.map (const 1)) 
                                      (\m -> assert (Map.null m) Map.empty) 
                                      m1 m2 