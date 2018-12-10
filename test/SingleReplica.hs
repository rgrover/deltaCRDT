{-# LANGUAGE TemplateHaskell #-}
module SingleReplica (runTests) where

import           CRDT.CRDT

import           Impl.KVStore.Pid
import           Impl.KVStore.ReplicatedKVStore

import           Test.QuickCheck

-- initialize with arbitrary Pid
--   * check replica's pid
--   * check than an arbitrary query fails
prop_initialize :: Word -> Int -> Bool
prop_initialize n key =
    and [pid state == P n, query state key == Nothing]
  where
    state :: AggregateState (ReplicatedKVStore Int String)
    state = initialize (P n)

-- initialize with Pid 1; and insert an arbitrary k-v pair
prop_singleInsert :: Int -> String -> Bool
prop_singleInsert key value = True
  where
    state :: AggregateState (ReplicatedKVStore Int String)
    state = initialize (P 1)

prop_singleInsertFollowedByQuery :: Int -> Bool
prop_singleInsertFollowedByQuery i = True

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
