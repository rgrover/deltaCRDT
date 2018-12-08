{-# LANGUAGE GADTs #-}
module CRDT.Algorithms where

import           CRDT.CvRDT       (CvRDT (..))
import           CRDT.DeltaCvRDT  (DeltaCvRDT (..))

import           CRDT.VectorClock as VectorClock (VectorClock (..),
                                                  increment, max)

import           Algebra.Lattice  (BoundedJoinSemiLattice, bottom,
                                   (\/))
import           Data.Map.Strict  as Map (Map, empty, findWithDefault,
                                          insertWith, toList)
import           Data.Sequence    as Seq (Seq (..),
                                          ViewR ((:>), EmptyR),
                                          dropWhileL, empty,
                                          foldlWithIndex, viewr, (><),
                                          (|>))

-- A sequence of deltas tagged with vector-clocks. This is used to
-- exchange collections of deltas between processes, and also to
-- maintain a local copy of deltas waiting to be disseminated.
--
-- Note: Delta-Intervals may be held in volatile storage.
type DeltaInterval s = Seq (VectorClock s, s)

-- Each process i keeps an acknowledgment map Ai that stores, for each
-- neighbor j, the largest clock b for all delta-intervals
-- acknowledged by j. Ai[i] should match a process's own vector clock.
-- Note: this map may be held in volatile storage.
type AckMap s = Map (ReplicaId s) (VectorClock s)

data AggregateState s where
    AggregateState ::
         DeltaCvRDT s => 
             { getOwnId  :: ReplicaId s
             , getS      :: s
             , getClock  :: VectorClock s
             , getDeltas :: DeltaInterval s -- delta group buffered for xmit
             , getAckMap :: AckMap s
             } -> AggregateState s

-- A message between two processes can either hold Deltas along with
-- clocks--i.e. DeltaInterval--or it can be an acknowledgement for
-- previously sent deltas
data Message s where
    Deltas :: DeltaCvRDT s => ReplicaId s -> DeltaInterval s -> Message s
    Ack :: DeltaCvRDT s => ReplicaId s -> VectorClock s -> Message s

-- Initialize δCvRDT
initDeltaCvRDTState :: DeltaCvRDT s => ReplicaId s -> AggregateState s
initDeltaCvRDTState ownId =
    AggregateState
    { getOwnId  = ownId
    , getS      = bottom
    , getClock  = bottom
    , getDeltas = Seq.empty
    , getAckMap = Map.empty
    }

-- To be called to perform an operation on the CvRDT state.
--
--   ci :: VectorClock   // local clock
--   Di :: DeltaInterval // localDeltas i.e. [(clock, delta)]
--   Xi :: CvRDT-state
--
-- pseudocode:
--   d = mδ(Xi)
--   X′i = Xi ⊔ d
--   D′i = Di { ci → d }
--   c′i = ci + 1
--
onOperation ::
       DeltaCvRDT s
    => OpsType s
    -> KeyType s
    -> ValueType s
    -> AggregateState s
    -> AggregateState s
onOperation op key value aggregateState =
    aggregateState {getS = x', getClock = clock', getDeltas = deltas'}
  where
    ownId   = getOwnId aggregateState
    x       = getS aggregateState
    clock   = getClock aggregateState
    deltas  = getDeltas aggregateState
    d       = deltaMutation op key value x
    x'      = x \/ d
    clock'  = increment clock ownId
    deltas' = deltas |> (clock', d)

onReceive ::
       DeltaCvRDT s
    => ReplicaId s
    -> Message s
    -> AggregateState s
    -> AggregateState s
-- To be called upon receiving an Ack message from a neighbour
-- input: message ~ ( ACK senderId remoteClock)
--
-- A′i = Ai { j → max ( Ai(j), remoteClock ) }
onReceive _ (Ack senderId receivedRemoteClock) aggregateState =
    aggregateState {getAckMap = aMap'}
  where
    aMap  = getAckMap aggregateState
    aMap' = updateAckMap senderId receivedRemoteClock aMap

-- To be called upon receiving a delta-interval from a neighbour
-- input : message ~ ( Deltas senderId deltas)
--
-- pseudocode:
--
-- if deltas not ⊑ Xi // i.e. sender's latest clock isn't <= ownClock
--   then
--     X′i = Xi ⊔ d        // incorporate deltas
--     c′i = ci + 1        // increment clock
--     D′i = Di { ci → d } // merge new deltas with local delta-group
--     sendTo senderId ( ACK finalClock )
--
onReceive ownId (Deltas senderId deltas) aggregateState =
    if sendersLatestClock <= ownClock
        then aggregateState -- deltas contain nothing new
        else aggregateState
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
    tailEndOfDeltas    = viewr deltas
    sendersLatestClock =
        case tailEndOfDeltas of
            EmptyR                -> bottom -- no deltas received
            _ :> (remoteClock, _) -> remoteClock

    usefulDeltas = deltas `unknownTo` ownClock
    ownDeltas'   = ownDeltas >< usefulDeltas

    (finalClock, finalState) =
        foldlWithIndex mergeDeltaWithState (ownClock', x) usefulDeltas
    mergeDeltaWithState ::
           DeltaCvRDT s
        => (VectorClock s, s)
        -> Int
        -> (VectorClock s, s)
        -> (VectorClock s, s)
    mergeDeltaWithState c1s1 _ c2s2 = c1s1 \/ c2s2

-- This method prepares a message to be sent to a neighbour. The user
-- of this library is expected to call this method periodically in a
-- manner which ensures eventual consistency--for instance, by sending
-- a message to all, a subset, or a randomly selected neighbour.
--
--   ci :: VectorClock // this is the local clock
--   Di :: DeltaInterval // i.e. [(clock, delta)]
--   Xi :: CvRDT-state
--   Ai :: AckMap
--   j :: Pid
--   j = RandomNeighbour // say
--
-- pseudocode:
--
--   if ci <= Ai(j)
--       then return (Deltas {})
--       else
--           if Di = {} ∨ min(domain(Di)) > Ai(j) then
--               d = Xi -- TODO:
--           else
--               d = ⊔ { Di(l) | Ai(j) ≤ l }
--
--           return (Deltas, d, ci)
periodicSendTo ::
       DeltaCvRDT s
    => AggregateState s
    -> ReplicaId s
    -> Maybe (Message s)
periodicSendTo aggregateState receiver =
    if ownClock <= knownRemoteClock
        then Nothing
        else Just (Deltas receiver relevantDeltas)
  where
    ownClock         = getClock aggregateState
    ackMap           = getAckMap aggregateState
    knownRemoteClock = findWithDefault bottom receiver ackMap
    deltas           = getDeltas aggregateState
    relevantDeltas   = deltas `unknownTo` knownRemoteClock

-- The user of this library needs to call this method periodically to
-- garbage collect local deltas by throwing away entries which have
-- been disseminated to all neighbours:
--
--   Di :: DeltaInterval
--   Ai :: AckMap
--
-- pseudocode:
--   minClock  = min { Cj | Cj ∈ Ai } :: VectorClock
--   Di′ = { ( clock, delta) ∈ Di | clock ≥ minClock }
periodicGarbageCollect ::
       DeltaCvRDT s => AggregateState s -> AggregateState s
periodicGarbageCollect aggregate = aggregate { getDeltas = deltas' }
  where
    aMap          = getAckMap aggregate
    (_, anyClock) = head $ Map.toList aMap
    minClock      = foldl min anyClock aMap
    deltas        = getDeltas aggregate
    deltas'       = deltas `unknownTo` minClock

-- helper function to update the AcknowledgementMap
updateAckMap ::
       DeltaCvRDT s
    => ReplicaId s
    -> VectorClock s
    -> AckMap s
    -> AckMap s
updateAckMap = insertWith VectorClock.max

-- helper function to select potentially interesting deltas from an
-- deltaInterval
unknownTo ::
       DeltaCvRDT s
    => DeltaInterval s
    -> VectorClock s
    -> DeltaInterval s
unknownTo ds c = dropWhileL ((<= c) . fst) ds
