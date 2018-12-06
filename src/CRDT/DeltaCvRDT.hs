{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}
module CRDT.DeltaCvRDT
    ( DeltaCvRDT
    , initDeltaCvRDTState
    , AggregateState
    ) where

import           CRDT.CvRDT
import           Misc.Pid
import           Misc.VectorClock                 as VC

import           Algebra.Lattice                  (BoundedJoinSemiLattice,
                                                   bottom, (\/))

import qualified Data.Map.Strict                  as Map
import qualified Data.Sequence                    as Seq

import           Control.Monad.Trans.State.Strict as S

-- A delta-interval-based, convergent replicated data types capable of
-- disseminating delta states (instead of complete clones) in order to achieve
-- eventual consistency.
--
--  s :: CRDT state forming a semilattice, where modifications are
--          inflationary and join is conflict-free.
--
-- In a δ-CRDT, the effect of applying a mutation, represented by a
-- delta-mutation δ = mδ(X), is decoupled from the resulting state X′ = X ⊔ δ,
-- which allows shipping this δ rather than the entire resulting state X′
class CvRDT s => DeltaCvRDT s where
    -- A delta-mutator mδ is a function, corresponding  to  an  update
    -- operation,  which  takes  a  state X in  a  join-semilattice S as
    -- parameter and returns a delta-mutation mδ(X), also in S.
    --
    -- The state transition at each replica is given by either joining the
    -- current state X ∈ S with a delta-mutation:
    --     X′ = X ⊔ mδ(X)
    -- or joining the current state with some received delta-group D:
    --     X′ = X ⊔ D.
    --
    -- Furthermore, if m is the corresponding state modifier of a CvRDT, then
    --     m(X) = X ⊔ mδ(X)
    --
    -- Note: a δ is just a state, that can be joined possibly several times
    -- without requiring exactly-once delivery, and without being a
    -- representation of the “increment” operation (as in operation-based
    -- CRDTs), which is itself non-idempotent;
    --
    -- Note: Pid parameter is used for passing own process Id
    deltaMutation :: Pid -> Ops s -> KeyType s -> ValueType s -> s -> s

    initDeltaCvRDTState :: DeltaCvRDT s => AggregateState s
    initDeltaCvRDTState =
        AggregateState
        { getS      = bottom
        , getClock  = bottom
        , getDeltas = Seq.empty
        , getAckMap = Map.empty
        }

    onOperation ::
           Pid
        -> Ops s
        -> KeyType s
        -> ValueType s
        -> AggregateState s
        -> AggregateState s
    onOperation ownId op key value aggregateState =
        aggregateState {getS = x', getClock = clock', getDeltas = deltas'}
      where
        x = getS aggregateState
        clock = getClock aggregateState
        deltas = getDeltas aggregateState
        d = deltaMutation ownId op key value x
        x' = x \/ d
        clock' = increment clock ownId
        deltas' = deltas Seq.|> (clock', d)

    onReceive :: Message s -> AggregateState s -> AggregateState s
    onReceive (Ack remoteId receivedRemoteClock) aggregateState =
        aggregateState {getAckMap = aMap'}
      where
        aMap = getAckMap aggregateState
        aMap' = Map.insertWith VC.max remoteId receivedRemoteClock aMap
    onReceive (Delta remoteId (deltas)) aggregateState = undefined

    periodicSendTo :: AggregateState s -> Pid -> DeltaInterval s
    periodicSendTo = undefined

    periodicGarbageCollect :: AggregateState s -> AggregateState s
    periodicGarbageCollect = undefined

-- A sequence of deltas tagged with vector-clocks. This is used to exchange
-- deltas between processes, and also to maintain a local copy of deltas
-- waiting to be disseminated.
--
-- Note: Delta-Intervals may be held in volatile storage.
type DeltaInterval s = Seq.Seq (VectorClock, s)

-- Each process i keeps an acknowledgment map Ai that stores, for each
-- neighbor j, the largest clock b for all delta-intervals acknowledged
-- by j. Ai[i] should match a process's own vector clock.
-- Note: this map may be held in volatile storage.
type AcknowledgementMap = Map.Map Pid VectorClock

-- Application facing state
data AggregateState s where
    AggregateState ::
         --DeltaCvRDT s => TODO: hindent fails to parse this: find workaround
             { getS      :: s
             , getClock  :: VectorClock
             , getDeltas :: DeltaInterval s
             , getAckMap :: AcknowledgementMap
             } -> AggregateState s

data Message s
    = Delta Pid (DeltaInterval s)
    | Ack Pid VectorClock

deltasFollowing :: VectorClock -> DeltaInterval s -> DeltaInterval s
deltasFollowing c deltas = undefined
