{-# LANGUAGE TemplateHaskell #-}
module SingleReplica (runTests) where

import           CRDT.CRDT

import           Impl.KVStore.Pid
import           Impl.KVStore.ReplicatedKVStore (KVStoreOps (..),
                                                 ReplicatedKVStore (..))

import           Algebra.Lattice                (bottom)

import           Data.Maybe                     (isNothing)
import           Test.QuickCheck

-- initialize with arbitrary Pid
--   * check replica's pid
--   * check than an arbitrary query fails
prop_initialize :: Word -> Int -> Bool
prop_initialize n key =
    pid state == P n
    && isNothing(query state key)
  where
    state :: AggregateState (ReplicatedKVStore Int String)
    state = initialize (P n)

-- initialize with Pid 1; and insert an arbitrary k-v pair
--   * lookup should succeed
--   * lookup with a different key should fail
--   * replica's clock is not bottom
prop_singleInsert :: Int -> Int -> String -> Property
prop_singleInsert key key' value =
    (key /= key') ==>
        query state' key == Just value
        && isNothing (query state' key') -- lookup of a different key
        && bottom < clock state'
  where
    state :: AggregateState (ReplicatedKVStore Int String)
    state  = initialize (P 1)
    state' = modify Add key value state

-- insert an arbitrary k-v pair and then remove it
--   * lookup following the remove should yield Nothing
--   * replica's clocks should be increasing
prop_insertFollowedByRemove :: Int -> String -> Bool
prop_insertFollowedByRemove key value =
        isNothing (query state'' key)
        && c < c'
        && c' < c''
  where
    state :: AggregateState (ReplicatedKVStore Int String)
    state   = initialize (P 1)
    c       = clock state
    state'  = modify Add key value state
    c'      = clock state'
    state'' = modify Remove key value state'
    c''     = clock state''

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
