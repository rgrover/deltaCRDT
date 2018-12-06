{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
module Misc.VectorClock (VectorClock, increment, max) where

import           Prelude          hiding (max)

import qualified Data.VectorClock as VC
import           Misc.Pid         (Pid (..))

import           Algebra.Lattice  (BoundedJoinSemiLattice,
                                   JoinSemiLattice, bottom, (\/))

import           Data.Word        (Word64)

-- type for process local logical time
newtype EventCount =
    E Word64
    deriving (Eq, Ord, Show, Num)

-- type of vector-clock to help establish causal dependency
type VectorClock = VC.VectorClock Pid EventCount

instance JoinSemiLattice VectorClock where
    (\/) :: VectorClock -> VectorClock -> VectorClock
    (\/) = VC.max

instance BoundedJoinSemiLattice VectorClock where
    bottom :: VectorClock
    bottom = VC.empty

instance Ord VectorClock where
    c1 <= c2 = (c1 == c2) || (c1 `VC.relation` c2 == VC.Causes)

increment :: VectorClock -> Pid -> VectorClock
increment c ownId = VC.incWithDefault ownId c 0

max :: VectorClock -> VectorClock -> VectorClock
max = VC.max
