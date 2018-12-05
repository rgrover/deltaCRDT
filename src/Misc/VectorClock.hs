{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
module Misc.VectorClock (VectorClock) where

import qualified Data.VectorClock as VC
import           Misc.Pid         (Pid (..))

import           Algebra.Lattice  (BoundedJoinSemiLattice, JoinSemiLattice,
                                   bottom, (\/))

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
