{-# LANGUAGE GADTs               #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module ReplicatedKVStore where

import           CRDT.CvRDT       (CvRDT (..))
import           CRDT.DeltaCvRDT  (DeltaCvRDT (..))
import           CRDT.VectorClock (VectorClock)

import           Algebra.Lattice  (BoundedJoinSemiLattice (..),
                                   JoinSemiLattice (..))

import           Data.Map         as Map (Map (..), empty, lookup,
                                          unionWith)

import           Data.List        (minimumBy)
import           Data.Ord         (comparing)
import           Data.Word        (Word64)

type Clock k v   = VectorClock (ReplicatedKVStore k v)
type PValue k v  = (Pid, Clock k v, v)
type NValue k v  = Clock k v

data ReplicatedKVStore k v =
    Store { getPSet :: Map k [PValue k v]
          , getNSet :: Map k [NValue k v]
          }

-- type for ProcessID
newtype Pid =
    P Word
    deriving (Eq, Ord, Show)

-- type for operations permitted on the KVStore
data KVStoreOps =
    Add | Remove
    deriving (Eq, Show)

instance Ord k => JoinSemiLattice (ReplicatedKVStore k v) where
    s1 \/ s2 =
        Store
        { getPSet = unionWith (++) (getPSet s1) (getPSet s2)
        , getNSet = unionWith (++) (getNSet s1) (getNSet s2)
        }

instance Ord k => BoundedJoinSemiLattice (ReplicatedKVStore k v) where
    bottom = Store { getPSet = Map.empty, getNSet = Map.empty }

instance Ord k => CvRDT (ReplicatedKVStore k v) where
    type ReplicaId (ReplicatedKVStore k v) = Pid
    type OpsType (ReplicatedKVStore k v)   = KVStoreOps
    type KeyType (ReplicatedKVStore k v)   = k
    type ValueType (ReplicatedKVStore k v) = v

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
          minus :: [PValue k v] -> Maybe [NValue k v] -> [PValue k v]
          minus ps Nothing   = ps
          minus ps (Just ns) = filter (not . (`shadowedBy` ns)) ps
            where
                shadowedBy :: PValue k v -> [NValue k v] -> Bool
                shadowedBy (_, pclock, _) = any (pclock <=)

    modify ::
           KVStoreOps
        -> k
        -> v
        -> ReplicatedKVStore k v
        -> ReplicatedKVStore k v
    modify = undefined

instance Ord k => DeltaCvRDT (ReplicatedKVStore k v) where
    type EventCounter (ReplicatedKVStore k v) = Word64

    deltaMutation ::
           KVStoreOps
        -> k
        -> v
        -> ReplicatedKVStore k v
        -> ReplicatedKVStore k v
    deltaMutation Add key value store    = undefined
    deltaMutation Remove key value store = undefined
