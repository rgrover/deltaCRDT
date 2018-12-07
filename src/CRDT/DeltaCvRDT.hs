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
import           Misc.VectorClock as VectorClock (VectorClock (..), increment,
                                                  max)

import           Algebra.Lattice  (BoundedJoinSemiLattice, bottom, (\/))

import           Data.Map.Strict  as Map (Map, empty, findWithDefault,
                                          insertWith)
import           Data.Sequence    as Seq (Seq (..), ViewR ((:>), EmptyR),
                                          dropWhileL, empty, foldlWithIndex,
                                          viewr, (><), (|>))

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

-- A sequence of deltas tagged with vector-clocks. This is used to
-- exchange collections of deltas between processes, and also to
-- maintain a local copy of deltas waiting to be disseminated.
--
-- Note: Delta-Intervals may be held in volatile storage.
type DeltaInterval s = Seq (VectorClock, s)

-- Each process i keeps an acknowledgment map Ai that stores, for each
-- neighbor j, the largest clock b for all delta-intervals
-- acknowledged by j. Ai[i] should match a process's own vector clock.
-- Note: this map may be held in volatile storage.
type AckMap = Map Pid VectorClock

data AggregateState s where
    AggregateState ::
         --DeltaCvRDT s => --TODO: hindent fails to parse this: find workaround
             { getS      :: s
             , getClock  :: VectorClock
             , getDeltas :: DeltaInterval s
             , getAckMap :: AckMap
             } -> AggregateState s

-- A message between two processes can either hold Deltas along with
-- clocks--i.e. DeltaInterval--or it can be an acknowledgement for
-- previously sent deltas
data Message s
    = Deltas Pid
            (DeltaInterval s)
    | Ack Pid
          VectorClock

-- Initialize δCvRDT
initDeltaCvRDTState :: DeltaCvRDT s => AggregateState s
initDeltaCvRDTState =
    AggregateState
    { getS      = bottom
    , getClock  = bottom
    , getDeltas = Seq.empty
    , getAckMap = Map.empty
    }

onOperation ::
       DeltaCvRDT s
    => Pid
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

onReceive ::
       DeltaCvRDT s
    => Pid
    -> Message s
    -> AggregateState s
    -> AggregateState s
onReceive _ (Ack senderId receivedRemoteClock) aggregateState =
    aggregateState {getAckMap = aMap'}
  where
    aMap  = getAckMap aggregateState
    aMap' = updateAckMap senderId receivedRemoteClock aMap

onReceive ownId (Deltas senderId deltas) aggregateState =
    if sendersLatestClock <= ownClock
        then aggregateState -- deltas contain nothing new
        else AggregateState
                 { getS      = finalState
                 , getClock  = finalClock
                 , getDeltas = ownDeltas'
                 , getAckMap = updateAckMap senderId sendersLatestClock aMap
                 }
  where
    x                  = getS aggregateState
    ownClock           = getClock aggregateState
    ownDeltas          = getDeltas aggregateState
    ownClock'          = increment ownClock ownId
    aMap               = getAckMap aggregateState
    tailEndOfDeltas    = Seq.viewr deltas
    sendersLatestClock =
        case tailEndOfDeltas of
            Seq.EmptyR                -> bottom -- no deltas received
            _ Seq.:> (remoteClock, _) -> remoteClock

    usefulDeltas   = deltas `unknownTo` ownClock
    ownDeltas'     = ownDeltas Seq.>< usefulDeltas

    (finalClock, finalState) =
        Seq.foldlWithIndex mergeDeltaWithState (ownClock', x) usefulDeltas
    mergeDeltaWithState ::
           DeltaCvRDT s
        => (VectorClock, s)
        -> Int
        -> (VectorClock, s)
        -> (VectorClock, s)
    mergeDeltaWithState c1s1 _ c2s2 = c1s1 \/ c2s2

periodicSendTo :: DeltaCvRDT s => AggregateState s -> Pid -> Message s
periodicSendTo aggregateState receiver = Deltas receiver relevantDeltas
    where
        ackMap           = getAckMap aggregateState
        knownRemoteClock = findWithDefault bottom receiver ackMap
        deltas           = getDeltas aggregateState
        relevantDeltas   = deltas `unknownTo` knownRemoteClock

periodicGarbageCollect :: DeltaCvRDT s => AggregateState s -> AggregateState s
periodicGarbageCollect = undefined

-- helper function to update the AcknowledgementMap
updateAckMap :: Pid -> VectorClock -> AckMap -> AckMap
updateAckMap = insertWith VectorClock.max

-- helper function to select potentially interesting deltas from an
-- deltaInterval
unknownTo :: DeltaInterval s -> VectorClock -> DeltaInterval s
unknownTo ds c = Seq.dropWhileL ((<= c) . fst) ds
