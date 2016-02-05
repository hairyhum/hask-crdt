{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Data.Crdt.ORSWOT where

import Data.Crdt.VersionVector
import Data.Crdt.Version
import Data.Crdt.CRSet
import Data.Crdt

import qualified Data.Map as Map

data ORSWOT v actor version = ORSWOT { 
    actor :: actor, 
    version :: version, 
    values :: Map.Map v version
}


instance (Ord v, Version version actor) => CRSet (ORSWOT v actor version) v where
    toSet       = Map.keysSet . values
    add ors val = 
        ors { version = new_version, 
              values  = Map.insert val (dot act new_version) vals}
        where
            act         = actor ors
            vals        = values ors
            new_version = increase act (version ors)
    remove ors val = 
        ors { values = Map.delete val (values ors)}

    union ors1 ors2 =
        ors1 { version = join vers1 vers2,
               values  = unite_vals ors1 ors2 }
        where
            vers1  = version ors1
            vers2  = version ors2
            vals1  = values ors1
            vals2  = values ors2
            common = Map.intersection vals1 vals2
            only1  = Map.difference vals1 vals2
            only2  = Map.difference vals2 vals1
            unite_vals ors1 ors2 = 
                Map.unions [common, 
                            (clean_diff vers2 only1),
                            (clean_diff vers1 only2)]
            clean_diff v = 
                Map.filter (not . (child v))


instance (Ord v, Version version actor) => Packable (ORSWOT v actor version) where
    pack ors1 ors2 =
        ors1 { version = packed_vers,
               values  = Map.map (\v -> if not (ancestor v packed_vers) 
                                        then dot act packed_vers
                                        else v) 
                                 (values ors1)}
        where 
            packed_vers = min_diff (version ors1) (version ors2)
            act         = (actor ors1)









