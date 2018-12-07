{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
module Misc.VectorClock
    ( VectorClock
    , increment
    , max
    , Misc.VectorClock.min -- qualified to avoid conflicting with Prelude
    ) where
import           Prelude          hiding (max)

import qualified Data.VectorClock as VC (Relation (..),
                                         VectorClock (..), combine,
                                         empty, incWithDefault, max,
                                         relation)
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

-- increment a process-specific component of a vector clock.
increment :: VectorClock -> Pid -> VectorClock
increment c ownId = VC.incWithDefault ownId c 0

-- The maximum of two vector-clocks; adding entries if absent in
-- either of the two clocks.
max :: VectorClock -> VectorClock -> VectorClock
max = VC.max

-- The minimum of the two vector clocks; discarding any entries which
-- aren't present in both clocks
min :: VectorClock -> VectorClock -> VectorClock
min = VC.combine minEntry
  where
    minEntry _ Nothing Nothing   = Nothing
    minEntry _ _ Nothing         = Nothing
    minEntry _ Nothing _         = Nothing
    minEntry _ (Just x) (Just y) = Just (Prelude.min x y)
