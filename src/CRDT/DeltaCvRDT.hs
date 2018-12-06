{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
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
-- disseminating delta states (instead of complete clones) in order to
-- achieve eventual consistency.
--
--  s :: CRDT state forming a semilattice, where modifications are
--       inflationary and join is conflict-free.
--
-- In a δ-CRDT, the effect of applying a mutation, represented by a
-- delta-mutation δ = mδ(X), is decoupled from the resulting state X′
-- = X ⊔ δ, which allows shipping this δ rather than the entire
-- resulting state X′
class CvRDT s => DeltaCvRDT s where
    -- A delta-mutator mδ is a function, corresponding  to  an  update
    -- operation,  which  takes  a  state X in  a  join-semilattice S
    -- as parameter and returns a delta-mutation mδ(X), also in S.
    --
    -- The state transition at each replica is given by either joining
    -- the current state X ∈ S with a delta-mutation: X′ = X ⊔ mδ(X)
    -- or joining the current state with some received delta-group D:
    --     X′ = X ⊔ D.
    --
    -- Furthermore, if m is the corresponding state modifier of a
    -- CvRDT, then
    --     m(X) = X ⊔ mδ(X)
    --
    -- Note: a δ is just a state, that can be joined possibly several
    -- times without requiring exactly-once delivery, and without
    -- being a representation of the “increment” operation (as in
    -- operation-based CRDTs), which is itself non-idempotent;
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
        x       = getS aggregateState
        clock   = getClock aggregateState
        deltas  = getDeltas aggregateState
        d       = deltaMutation ownId op key value x
        x'      = x \/ d
        clock'  = increment clock ownId
        deltas' = deltas Seq.|> (clock', d)

    onReceive :: Pid -> Message s -> AggregateState s -> AggregateState s
    onReceive _ (Ack senderId receivedRemoteClock) aggregateState =
        aggregateState {getAckMap = aMap'}
      where
        aMap  = getAckMap aggregateState
        aMap' = updateAckMap senderId receivedRemoteClock aMap
    onReceive ownId (Delta senderId deltas) aggregateState =
        case tailEndOfDeltas of
            Seq.EmptyR                -> aggregateState -- no deltas received
            _ Seq.:> (latestClock, _) ->
                if latestClock <= ownClock
                    then aggregateState -- deltas contain nothing new
                    else AggregateState
                         { getS = undefined
                         , getClock = ownClock'
                         , getDeltas = undefined
                         , getAckMap = updateAckMap senderId latestClock aMap
                         }
      where
          x               = getS aggregateState
          ownClock        = getClock aggregateState
          deltas          = getDeltas aggregateState
          ownClock'       = increment ownClock ownId
          aMap            = getAckMap aggregateState
          tailEndOfDeltas = Seq.viewr deltas

    periodicSendTo :: AggregateState s -> Pid -> DeltaInterval s
    periodicSendTo = undefined

    periodicGarbageCollect :: AggregateState s -> AggregateState s
    periodicGarbageCollect = undefined

-- A sequence of deltas tagged with vector-clocks. This is used to
-- exchange deltas between processes, and also to maintain a local
-- copy of deltas waiting to be disseminated.
--
-- Note: Delta-Intervals may be held in volatile storage.
type DeltaInterval s = Seq.Seq (VectorClock, s)

-- Each process i keeps an acknowledgment map Ai that stores, for each
-- neighbor j, the largest clock b for all delta-intervals
-- acknowledged by j. Ai[i] should match a process's own vector clock.
-- Note: this map may be held in volatile storage.
type AckMap = Map.Map Pid VectorClock

data AggregateState s where
    AggregateState ::
         --DeltaCvRDT s => TODO: hindent fails to parse this: find workaround
             { getS      :: s
             , getClock  :: VectorClock
             , getDeltas :: DeltaInterval s
             , getAckMap :: AckMap
             } -> AggregateState s

-- A message between two processes can either hold Deltas along with
-- clocks--i.e. DeltaInterval--or it can be an acknowledgement for
-- previously sent deltas
data Message s
    = Delta Pid
            (DeltaInterval s)
    | Ack Pid
          VectorClock

deltasFollowing :: VectorClock -> DeltaInterval s -> DeltaInterval s
deltasFollowing c deltas = undefined

-- helper function to update the AcknowledgementMap
updateAckMap :: Pid -> VectorClock -> AckMap -> AckMap
updateAckMap = Map.insertWith VC.max
