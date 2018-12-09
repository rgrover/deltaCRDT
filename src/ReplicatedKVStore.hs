{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ReplicatedKVStore where

import           CRDT.CvRDT      (CvRDT (..))
import           CRDT.DeltaCvRDT (DeltaCvRDT (..))

import           Pid
import           VectorClock

import           Algebra.Lattice (BoundedJoinSemiLattice (..),
                                  JoinSemiLattice (..))

import           Data.Map        as Map (Map (..), empty, lookup,
                                         unionWith)

import           Data.List       (minimumBy)
import           Data.Ord        (comparing)


type Clock    = VectorClock.VectorClock
type PValue v = (Pid, Clock, v)--P-set{key} is a collection of this value-type
type NValue   = Clock          --N-set{key} is a collection of this value-type

data ReplicatedKVStore k v = Store
    { getClock :: Clock
    , getOwnId :: Pid
    , getPSet  :: Map k [PValue v]
    , getNSet  :: Map k [NValue]
    }

-- type for operations permitted on the KVStore
data KVStoreOps =
    Add | Remove
    deriving (Eq, Show)

instance Ord k => JoinSemiLattice (ReplicatedKVStore k v) where
    s1 \/ s2 =
        Store
        { getClock = getClock s1 \/ getClock s2
        , getOwnId = getOwnId s1 -- we assume that s2 is a delta mutation
        , getPSet  = unionWith (++) (getPSet s1) (getPSet s2)
        , getNSet  = unionWith (++) (getNSet s1) (getNSet s2)
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
        Store { getClock = bottom
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
            [(_, _, x)] -> Just x -- got a single value
            xs          ->        -- got a collection of concurrent values
                let (_, _, value) =
                        minimumBy (comparing (\(id, _, _) -> id)) xs
                in Just value
      where
          minus :: [PValue v] -> Maybe [NValue] -> [PValue v]
          minus ps Nothing   = ps
          minus ps (Just ns) = filter (not . (`shadowedBy` ns)) ps
            where
                shadowedBy :: PValue v -> [NValue] -> Bool
                shadowedBy (_, pclock, _) = any (pclock <=)

    modify ::
           KVStoreOps
        -> k
        -> v
        -> ReplicatedKVStore k v
        -> ReplicatedKVStore k v
    modify = undefined

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
        c'    = VectorClock.increment ownId c

    deltaMutation ::
           KVStoreOps
        -> k
        -> v
        -> ReplicatedKVStore k v
        -> ReplicatedKVStore k v
    deltaMutation Add key value store    = undefined
    deltaMutation Remove key value store = undefined
