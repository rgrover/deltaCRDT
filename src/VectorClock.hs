{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module VectorClock (VectorClock, increment) where

import           Pid

import           Algebra.Lattice  (BoundedJoinSemiLattice (..),
                                   BoundedLattice (..),
                                   JoinSemiLattice (..),
                                   MeetSemiLattice (..))
import qualified Data.VectorClock as VC (Relation (..),
                                         VectorClock (..), combine,
                                         empty, incWithDefault, max,
                                         relation)

import           Data.Word        (Word64)

type EventCounter = Word64
type VectorClock  = VC.VectorClock Pid EventCounter

instance JoinSemiLattice VectorClock where
    (\/) :: VectorClock -> VectorClock -> VectorClock
    (\/) = VC.max

instance BoundedJoinSemiLattice VectorClock where
    bottom :: VectorClock
    bottom = VC.empty

instance MeetSemiLattice VectorClock where
    -- The minimum of the two vector clocks; discarding any entries which
    -- aren't present in both clocks
    (/\) :: VectorClock -> VectorClock -> VectorClock
    c1 /\ c2 = VC.combine minEntry c1 c2
      where
        minEntry _ Nothing Nothing   = Nothing
        minEntry _ _ Nothing         = Nothing
        minEntry _ Nothing _         = Nothing
        minEntry _ (Just x) (Just y) = Just (Prelude.min x y)

instance Ord VectorClock where
    c1 `compare` c2 =
        if c1 == c2
            then EQ
            else case (c1 `VC.relation` c2) of
                     VC.Causes     -> LT
                     VC.CausedBy   -> GT
                     VC.Concurrent -> EQ

-- increment a process-specific component of a vector clock.
increment :: Pid -> VectorClock -> VectorClock
increment ownId c = VC.incWithDefault ownId c 0
