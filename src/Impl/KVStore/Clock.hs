{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Impl.KVStore.Clock (Clock, singleton, increment, fromList) where

import           Impl.KVStore.Pid

import           Algebra.Lattice  (BoundedJoinSemiLattice (..),
                                   BoundedLattice (..),
                                   JoinSemiLattice (..),
                                   MeetSemiLattice (..))
import qualified Data.VectorClock as VC (Relation (..),
                                         VectorClock (..), combine,
                                         empty, fromList,
                                         incWithDefault, max,
                                         relation, singleton)

import           Data.Word        (Word64)

type EventCounter = Word64
type Clock  = VC.VectorClock Pid EventCounter

fromList :: [(Pid, EventCounter)] -> Clock
fromList = VC.fromList

instance JoinSemiLattice Clock where
    (\/) :: Clock -> Clock -> Clock
    (\/) = VC.max

instance MeetSemiLattice Clock where
    -- The minimum of the two vector clocks; discarding any entries
    -- which aren't present in both clocks
    (/\) :: Clock -> Clock -> Clock
    c1 /\ c2 = VC.combine minEntry c1 c2
      where
        minEntry _ Nothing Nothing   = Nothing
        minEntry _ _ Nothing         = Nothing
        minEntry _ Nothing _         = Nothing
        minEntry _ (Just x) (Just y) = Just (Prelude.min x y)

instance Ord Clock where
    c1 `compare` c2
        | c1 == c2 = EQ
        | otherwise =
            case (c1 `VC.relation` c2) of
                VC.Causes     -> LT
                VC.CausedBy   -> GT
                VC.Concurrent -> EQ

singleton :: Pid -> Clock
singleton ownId = VC.singleton ownId 1

-- increment a process-specific component of a vector clock.
increment :: Pid -> Clock -> Clock
increment ownId c = VC.incWithDefault ownId c 0
