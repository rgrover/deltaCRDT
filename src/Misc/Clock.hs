{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
module Misc.Clock (Clock) where

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
type Clock = VC.VectorClock Pid EventCount

instance JoinSemiLattice Clock where
    (\/) :: Clock -> Clock -> Clock
    (\/) = VC.max

instance BoundedJoinSemiLattice Clock where
    bottom :: Clock
    bottom = VC.empty
