{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
module CRDT.Core.AggregateState
    ( AggregateState(..)
    , DeltaInterval
    ) where

import           CRDT.Core.CvRDT      (ReplicaId)
import           CRDT.Core.DeltaCvRDT (DeltaCvRDT, VectorClock)

import           Data.Map.Strict      as Map (Map)
import           Data.Sequence        as Seq (Seq)

-- A sequence of deltas. This is used to exchange collections of
-- deltas between processes, and also to maintain a local copy of
-- deltas waiting to be disseminated.
--
-- Note: Delta-Intervals may be held in volatile storage.
type DeltaInterval s = Seq s

-- Each process i keeps an acknowledgment map Ai that stores, for each
-- neighbor j, the largest clock b for all delta-intervals
-- acknowledged by j. Ai[i] should match a process's own vector clock.
-- Note: this map may be held in volatile storage.
type AckMap s = Map (ReplicaId s) (VectorClock s)

data AggregateState s where
    AggregateState ::
        (DeltaCvRDT s, Show(ReplicaId s), Show s, Show (VectorClock s)) =>
        { getS :: s
        , getDeltas :: DeltaInterval s
        , getAckMap :: Map (ReplicaId s) (VectorClock s)
        } -> AggregateState s

deriving instance Show (AggregateState s)
