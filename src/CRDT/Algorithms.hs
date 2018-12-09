{-# LANGUAGE TypeFamilies #-}
module CRDT.Algorithms where

import           CRDT.CvRDT      (CvRDT (..))
import           CRDT.DeltaCvRDT (DeltaCvRDT (..))

import           Algebra.Lattice (bottom, (/\), (\/))
import           Data.Map.Strict as Map (Map, empty, findWithDefault,
                                         insertWith, toList)
import           Data.Sequence   as Seq (Seq, dropWhileL, empty,
                                         foldlWithIndex, null, (><),
                                         (|>))

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

data family AggregateState s :: *

data instance AggregateState s =
    AggregateState
        { getS      :: s
        , getDeltas :: DeltaInterval s
        , getAckMap :: AckMap s
        }

-- A message between two processes can either hold Deltas along with
-- clocks--i.e. DeltaInterval--or it can be an acknowledgement for
-- previously sent deltas
data Message s
    = Ack (ReplicaId s)        -- sender's id
          (VectorClock s)      -- sender's clock
    | Deltas (ReplicaId s)     -- sender's id
             (VectorClock s)   -- sender's clock
             (DeltaInterval s) -- deltas

-- Initialize δCvRDT
initDeltaCvRDTState :: DeltaCvRDT s => ReplicaId s -> AggregateState s
initDeltaCvRDTState ownId =
    AggregateState
    { getS      = initialize ownId
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
--   c′i = clock d
--   X′i = Xi ⊔ d // c'i is also clock X'i
--   D′i = Di { c'i → d }
--
onOperation ::
       DeltaCvRDT s
    => OpsType s
    -> KeyType s
    -> ValueType s
    -> AggregateState s
    -> AggregateState s
onOperation op key value aggregateState =
    aggregateState {getS = x', getDeltas = deltas'}
  where
    x       = getS aggregateState
    deltas  = getDeltas aggregateState
    d       = deltaMutation op key value x
    x'      = x \/ d
    deltas' = deltas |> d

onReceive ::
       DeltaCvRDT s
    => Message s
    -> AggregateState s
    -> AggregateState s
-- To be called upon receiving an Ack message from a neighbour
-- input: message ~ ( ACK senderId remoteClock)
--
-- A′i = Ai { j → max ( Ai(j), remoteClock ) }
onReceive (Ack senderId receivedRemoteClock) aggregateState =
    aggregateState {getAckMap = aMap'}
  where
    aMap  = getAckMap aggregateState
    aMap' = insertWith (\/) senderId receivedRemoteClock aMap

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
onReceive (Deltas senderId sendersClock deltas) aggregateState =
    if (sendersClock <= ownClock) || (Seq.null deltas)
        then aggregateState -- nothing to do
        else aggregateState
             { getS      = finalState
             , getDeltas = ownDeltas'
             , getAckMap = insertWith (\/) senderId sendersClock aMap
             }
  where
    x               = getS aggregateState
    ownPid          = pid x
    ownClock        = clock x
    x'              = incrementClock x
    ownDeltas       = getDeltas aggregateState
    aMap            = getAckMap aggregateState

    usefulDeltas = deltas `unknownTo` ownClock
    ownDeltas'   = ownDeltas >< usefulDeltas

    finalState = foldlWithIndex mergeDelta x' usefulDeltas
    mergeDelta :: DeltaCvRDT s => s -> Int -> s -> s
    mergeDelta s1 _ s2 = s1 \/ s2

-- Prepare an Ack message to be sent to a neighbour. The user
-- of this library is expected to send ACK messages periodically in a
-- manner which ensures eventual consistency--for instance, by sending
-- a message to all, a subset, or a randomly selected neighbour.
composeAckMessageTo ::
       DeltaCvRDT s
    => AggregateState s
    -> Message s
composeAckMessageTo aggregateState = Ack ownId c
    where x     = getS aggregateState
          ownId = pid x
          c     = clock x

-- Prepare a Delta message to be sent to a neighbour. The user
-- of this library is expected to send Delta messages periodically in a
-- manner which ensures eventual consistency--for instance, by sending
-- a message to all, a subset, or a randomly selected neighbour.
--
-- Note: This method generates Nothing if the receiver is known to
-- need nothing from the sender's local state. This can be used as an
-- indication to try another receiver or to throttle down
-- communication.
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
composeDeltasMessageTo ::
       DeltaCvRDT s
    => ReplicaId s
    -> AggregateState s
    -> Maybe (Message s)
composeDeltasMessageTo receiver aggregateState =
    if ownClock <= knownRemoteClock
        then Nothing
        else Just (Deltas ownId ownClock relevantDeltas)
  where
    x                = getS aggregateState
    ownId            = pid x
    ownClock         = clock x
    ackMap           = getAckMap aggregateState
    knownRemoteClock = findWithDefault bottom receiver ackMap
    deltas           = getDeltas aggregateState
    relevantDeltas   = deltas `unknownTo` knownRemoteClock

-- The user of this library should call this method periodically to
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
    minClock      = foldl (/\) anyClock aMap
    deltas        = getDeltas aggregate
    deltas'       = deltas `unknownTo` minClock

-- helper function to select potentially interesting deltas from an
-- deltaInterval
unknownTo ::
       DeltaCvRDT s
    => DeltaInterval s
    -> VectorClock s
    -> DeltaInterval s
unknownTo ds c = dropWhileL ((<= c) . clock) ds
