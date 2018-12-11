{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Impl.KVStore.ReplicatedKVStore where

import           CRDT.Core.CvRDT      (CvRDT (..))
import           CRDT.Core.DeltaCvRDT (DeltaCvRDT (..))

import           Impl.KVStore.Clock   as Clock
import           Impl.KVStore.Pid

import           Algebra.Lattice      (JoinSemiLattice (..),
                                       MeetSemiLattice (..))

import           Data.Map             as Map (Map (..), empty,
                                              fromList, insertWith,
                                              lookup, unionWith)

import           Data.Coerce          (coerce)
import           Data.List            (minimumBy, sort, sortBy)
import           Data.Ord             (Down (..), comparing)

type PValue v = (Pid, Clock, v)--P-set{key} is a collection of this value-type
type NValue   = Clock          --N-set{key} is a collection of this value-type

type PValueSet v = [PValue v]
type NValueSet   = [NValue]

data ReplicatedKVStore k v = Store
    { getClock :: Clock
    , getOwnId :: Pid
    , getPSet  :: Map k (PValueSet v)
    , getNSet  :: Map k NValueSet
    } deriving (Show)

-- type for operations permitted on the KVStore
data KVStoreOps =
    Add | Remove
    deriving (Eq, Show)

instance Ord k => JoinSemiLattice (ReplicatedKVStore k v) where
    s1 \/ s2 =
        Store
        { getClock = getClock s1 \/ getClock s2
        , getOwnId = getOwnId s1 -- we assume s2 is the remote state
        , getPSet  = unionWith mergePSets (getPSet s1) (getPSet s2)
        , getNSet  = unionWith mergeNSets (getNSet s1) (getNSet s2)
        }

instance Ord k => CvRDT (ReplicatedKVStore k v) where
    type ReplicaId (ReplicatedKVStore k v) = Pid
    type OpsType (ReplicatedKVStore k v)   = KVStoreOps
    type KeyType (ReplicatedKVStore k v)   = k
    type ValueType (ReplicatedKVStore k v) = v

    pid :: ReplicatedKVStore k v -> Pid
    pid = getOwnId

    initialize :: Pid -> ReplicatedKVStore k v
    initialize pid =
        Store { getClock = singleton pid -- init. with eventCounter 1
              , getOwnId = pid
              , getPSet  = Map.empty
              , getNSet  = Map.empty
              }

    query :: ReplicatedKVStore k v -> k -> Maybe v
    query store key = do
        ps <- Map.lookup key (getPSet store)
        let ns    = Map.lookup key (getNSet store)
            diffs = ps `minus` ns
        case diffs of
            []          -> Nothing
            [(_, _, v)] -> Just v -- got a single value
            xs          -> Just $ resolveConcurrency xs
      where
        minus :: PValueSet v -> Maybe NValueSet -> PValueSet v
        minus ps Nothing   = ps
        minus ps (Just ns) = filter (not . (`shadowedBy` ns)) ps
          where
            shadowedBy :: PValue v -> NValueSet -> Bool
            shadowedBy (_, pclock, _) = any (pclock <)
        -- This helper function resolves concurrency amongst the Add
        -- entries. It first sorts on the basis of descending clock
        -- to find the equivalence class with the most recent clock.
        -- From this equivalence class, an arbitrary choice is made
        -- based on smallest PID. Note: the comparison function for
        -- vectorClock treats concurrent clocks as EQ.
        resolveConcurrency :: PValueSet v -> v
        resolveConcurrency xs = value
          where
            equiv = bestPSubset xs
              -- take arbitrary minimum of the equivalence class based on pid
            (_, _, value) =
                minimumBy (comparing (\(id, _, _) -> id)) equiv
    modify ::
           KVStoreOps
        -> k
        -> v
        -> ReplicatedKVStore k v
        -> ReplicatedKVStore k v
    modify Add key value store = store' {getPSet = pSet'}
      where
        store' = incrementClock store
        clock' = getClock store'
        ownId  = getOwnId store
        entry  = [(ownId, clock', value)]
        pSet   = getPSet store
        pSet'  = Map.insertWith mergePSets key entry pSet

    modify Remove key _ store  = store' {getNSet = nSet'}
      where
        store' = incrementClock store
        clock' = getClock store'
        entry  = [clock']
        nSet   = getNSet store
        nSet'  = Map.insertWith mergeNSets key entry nSet

instance Ord k => DeltaCvRDT (ReplicatedKVStore k v) where

    type VectorClock (ReplicatedKVStore k v) = Clock

    -- fetch current clock from a given state
    clock :: ReplicatedKVStore k v -> Clock
    clock = getClock

    -- increment a replica-specific component of a vector-clock
    incrementClock :: ReplicatedKVStore k v -> ReplicatedKVStore k v
    incrementClock store = store {getClock = c'}
      where
        ownId = pid store
        c     = clock store
        c'    = Clock.increment ownId c

    updateClock :: Clock -> ReplicatedKVStore k v -> ReplicatedKVStore k v
    updateClock c store = store {getClock = c \/ ownClock}
      where
        ownClock = clock store

    deltaMutation ::
           KVStoreOps
        -> k
        -> v
        -> ReplicatedKVStore k v
        -> ReplicatedKVStore k v
    deltaMutation Add key value store =
        store' { getPSet = pSet, getNSet = Map.empty }
      where
        store' = incrementClock store
        clock' = getClock store'
        ownId  = getOwnId store
        entry  = [(ownId, clock', value)]
        pSet   = Map.fromList [(key, entry)]

    deltaMutation Remove key _ store =
        store' { getPSet = Map.empty, getNSet = nSet }
      where
        store' = incrementClock store
        clock' = getClock store'
        entry  = [clock']
        nSet   = Map.fromList [(key, entry)]

bestPSubset :: PValueSet v -> PValueSet v
bestPSubset xs = equiv
    -- coerce clocks as Down to get a reversed comparison function
  where
    toDown               = (\(pid, c, v) -> (pid, Down c, v))
    fromDown             = (\(pid, Down c, v) -> (pid, c, v))
    downXs               = map toDown xs
    sortedXs             = sortBy (comparing (\(_, c, _) -> c)) downXs
    sortedXs'            = map fromDown sortedXs
    (_, firstBestClk, _) = head sortedXs' -- first of the latest clocks
    -- find the equivalence class of concurrent clocks
    equiv =
        takeWhile
            (\(_, c, _) -> (c `compare` firstBestClk) == EQ)
            sortedXs'

mergePSets :: PValueSet v -> PValueSet v -> PValueSet v
mergePSets = (\a b -> bestPSubset (a ++ b))

bestNSubset :: NValueSet -> NValueSet
bestNSubset xs = equiv
    -- coerce clocks as Down to get a reversed comparison function
  where
    downXs       = map Down xs
    sortedXs     = sort downXs
    sortedXs'    = map (\ (Down x) -> x) sortedXs
    firstBestClk = head sortedXs' -- first of the latest clocks
    -- find the equivalence class of concurrent clocks
    equiv =
        takeWhile
            (\c -> (c `compare` firstBestClk) == EQ)
            sortedXs'

mergeNSets :: NValueSet -> NValueSet -> NValueSet
mergeNSets = (\a b -> bestNSubset (a ++ b))
