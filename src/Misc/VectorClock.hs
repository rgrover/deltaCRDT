{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Misc.VectorClock
    ( VectorClock
    , max
    , increment
    , Misc.VectorClock.min -- qualified to avoid conflicting with Prelude
    ) where
import           Prelude          hiding (max)

import           CRDT.CvRDT       (CvRDT (ReplicaId))
import           CRDT.DeltaCvRDT  (DeltaCvRDT (EventCounter))

import qualified Data.VectorClock as VC (Relation (..),
                                         VectorClock (..), combine,
                                         empty, incWithDefault, max,
                                         relation)

import           Algebra.Lattice  (BoundedJoinSemiLattice,
                                   JoinSemiLattice, bottom, (\/))

-- type of vector-clock to help establish causal dependency. This is
-- built atop the CRDT-specific event counter, using ReplicaId as the key.
newtype VectorClock s =
    VectorClock (VC.VectorClock (ReplicaId s) (EventCounter s))

instance DeltaCvRDT s => JoinSemiLattice (VectorClock s) where
    (\/) :: VectorClock s -> VectorClock s -> VectorClock s
    (VectorClock c1) \/ (VectorClock c2) = VectorClock $ VC.max c1 c2

instance DeltaCvRDT s => BoundedJoinSemiLattice (VectorClock s) where
    bottom :: VectorClock s
    bottom = VectorClock VC.empty

instance (DeltaCvRDT s, Eq (EventCounter s)) =>
         Eq (VectorClock s) where
    (VectorClock c1) == (VectorClock c2) = c1 == c2

instance DeltaCvRDT s => Ord (VectorClock s) where
    (VectorClock c1) <= (VectorClock c2) =
        (c1 == c2) || (c1 `VC.relation` c2 == VC.Causes)

-- increment a process-specific component of a vector clock.
increment :: DeltaCvRDT s => VectorClock s -> ReplicaId s -> VectorClock s
increment (VectorClock c) ownId =
    VectorClock $ VC.incWithDefault ownId c 0

-- The maximum of two vector-clocks; adding entries if absent in
-- either of the two clocks.
max :: DeltaCvRDT s => VectorClock s -> VectorClock s -> VectorClock s
max (VectorClock c1) (VectorClock c2) = VectorClock $ VC.max c1 c2

-- The minimum of the two vector clocks; discarding any entries which
-- aren't present in both clocks
min :: DeltaCvRDT s => VectorClock s -> VectorClock s -> VectorClock s
min (VectorClock c1) (VectorClock c2) =
    VectorClock $ VC.combine minEntry c1 c2
  where
    minEntry _ Nothing Nothing   = Nothing
    minEntry _ _ Nothing         = Nothing
    minEntry _ Nothing _         = Nothing
    minEntry _ (Just x) (Just y) = Just (Prelude.min x y)
