{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module CRDT.Core.Algorithms where

import           CRDT.Core.AggregateState (AggregateState (..),
                                           DeltaInterval)
import           CRDT.Core.CvRDT          as CvRDT (CvRDT (..))
import           CRDT.Core.DeltaCvRDT     as DeltaCvRDT (DeltaCvRDT (..),
                                                         clock)
import           CRDT.Core.Message

import           Algebra.Lattice          (bottom, (/\), (\/))
import           Data.Map.Strict          as Map (Map, empty,
                                                  findWithDefault,
                                                  insertWith, lookup,
                                                  toList)
import           Data.Sequence            as Seq (Seq, ViewL ((:<)),
                                                  dropWhileL, empty,
                                                  foldlWithIndex,
                                                  null, viewl, (><),
                                                  (|>))

import           Data.Maybe               (fromJust, isNothing)

-- Initialize δCvRDT
initialize ::
       (DeltaCvRDT s, Show s, Show (VectorClock s))
    => ReplicaId s
    -> AggregateState s
initialize ownId =
    AggregateState
    { getS      = CvRDT.initialize ownId
    , getDeltas = Seq.empty
    , getAckMap = Map.empty
    }

-- fetch replica-id
pid :: DeltaCvRDT s => AggregateState s -> ReplicaId s
pid = CvRDT.pid . getS

clock :: DeltaCvRDT s => AggregateState s -> VectorClock s
clock = DeltaCvRDT.clock . getS

-- query a key
query ::
       DeltaCvRDT s
    => AggregateState s
    -> KeyType s
    -> Maybe (ValueType s)
query = (CvRDT.query . getS)

-- To be called to perform an operation on the CvRDT state. The nature
-- of the operation is specific to the implementation.
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
modify ::
       DeltaCvRDT s
    => OpsType s
    -> KeyType s
    -> ValueType s
    -> AggregateState s
    -> AggregateState s
modify op key value aggregateState =
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

-- To be called upon receiving a delta-interval from a neighbour.
--
-- pseudocode:
--
-- if deltas not ⊑ Xi // i.e. sender's latest clock isn't < ownClock
--   then
--     X′i = Xi ⊔ d        // incorporate deltas
--     c′i = ci + 1        // increment clock
--     D′i = Di { ci → d } // merge new deltas with local delta-group
--     (optionally) sendTo senderId ( ACK finalClock )
onReceive (Deltas senderId sendersClock deltas) aggregateState =
    if (sendersClock < ownClock) || (Seq.null deltas)
        then aggregateState -- nothing to do
        else aggregateState
             { getS      = finalState
             , getDeltas = ownDeltas'
             , getAckMap = insertWith (\/) senderId sendersClock aMap
             }
  where
    x         = getS aggregateState
    ownPid    = CvRDT.pid x
    ownClock  = DeltaCvRDT.clock x
    x'        = incrementClock x
    x''       = updateClock sendersClock x'
    ownDeltas = getDeltas aggregateState
    aMap      = getAckMap aggregateState

    usefulDeltas = deltas `unknownTo` ownClock
    ownDeltas'   = ownDeltas >< usefulDeltas

    finalState = foldlWithIndex mergeDelta x'' usefulDeltas
    mergeDelta :: DeltaCvRDT s => s -> Int -> s -> s
    mergeDelta s1 _ s2 = s1 \/ s2

-- To be called upon receiving a state-dump from a neighbour
-- Input: Xr as the remote state
--
-- pseudocode:
--     c′i = ci + 1        // increment clock
--     X′i = Xi ⊔ Xr       // incorporate deltas
--     D′i = Di { ci → Xr } // merge new deltas with local delta-group
--     (optionally) sendTo senderId ( ACK finalClock )
onReceive (State rx) aggregateState =
    if rClock < ownClock
        then aggregateState -- received state contains old information
        else aggregateState
             { getS      = mergedState
             , getDeltas = deltas'
             , getAckMap = aMap'
             }
  where
    x           = getS aggregateState
    ownClock    = DeltaCvRDT.clock x
    rClock      = DeltaCvRDT.clock rx

    --     X′i = Xi ⊔ Xr       // incorporate deltas
    x'          = incrementClock x
    mergedState = x' \/ rx

    --     D′i = Di { ci → Xr }
    mergedClock = DeltaCvRDT.clock mergedState
    rx'         = DeltaCvRDT.updateClock mergedClock rx
    deltas      = getDeltas aggregateState
    deltas'     = deltas |> rx'

    remoteId    = CvRDT.pid rx
    aMap        = getAckMap aggregateState
    aMap'       = insertWith (\/) remoteId rClock aMap

-- Prepare an Ack message to be sent to a neighbour. The user
-- of this library is expected to send ACK messages periodically in a
-- manner which ensures eventual consistency--for instance, by sending
-- a message to all, a subset, or a randomly selected neighbour.
composeAckMessage ::
       (DeltaCvRDT s, Show s, Show (VectorClock s))
    => AggregateState s
    -> Message s
composeAckMessage aggregateState = Ack ownId c
    where x     = getS aggregateState
          ownId = CvRDT.pid x
          c     = DeltaCvRDT.clock x

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
--   j  :: Pid
--   j = RandomNeighbour // say
--
-- pseudocode:
--
--   if ci < Ai(j)
--       then return (Deltas {})
--       else
--           if receiver is unknown ∨ Di = {} ∨ (min-clock(Di) ≮ Ai(j)) then
--               d = Xi
--           else
--               d = ⊔ { Di(l) | Ai(j) ≤ l }
--
--           return (Deltas, d, ci)
composeMessageTo ::
       (DeltaCvRDT s, Show s, Show (VectorClock s))
    => ReplicaId s
    -> AggregateState s
    -> Maybe (Message s)
composeMessageTo receiver aggregateState =
    if Seq.null deltas ||
       isNothing knownRemoteClock ||
       not (DeltaCvRDT.clock minDelta < remoteClock)
        then Just $ State x -- send all state
        else let relevantDeltas = deltas `unknownTo` remoteClock
             in Just (Deltas ownId ownClock relevantDeltas)
  where
    x                = getS aggregateState
    ownId            = CvRDT.pid x
    ownClock         = DeltaCvRDT.clock x
    deltas           = getDeltas aggregateState
    ackMap           = getAckMap aggregateState
    knownRemoteClock = Map.lookup receiver ackMap
    remoteClock      = fromJust knownRemoteClock
    minDelta :< _    = viewl deltas

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
unknownTo ds c = dropWhileL ((< c) . DeltaCvRDT.clock) ds
