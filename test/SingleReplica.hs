{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module SingleReplica (runTests) where

import           CRDT.CRDT

import           Impl.KVStore.Pid
import           Impl.KVStore.ReplicatedKVStore (KVStoreOps (..), ReplicatedKVStore (..))

import           Algebra.Lattice                (bottom)

import           Data.List                      (foldl', nubBy, zipWith)
import qualified Data.Map                       as Map
import           Data.Maybe                     (isNothing)

import           Test.QuickCheck

-- initialize with arbitrary Pid
--   * check replica's pid
--   * check that an arbitrary query fails
prop_initialize :: Word -> Int -> Bool
prop_initialize n key =
    pid state == P n
    && isNothing (query state key)
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

-- insert several arbitrary values for a key in succession.
--   * lookup following the inserts should match the last value.
--   * replica's clocks should be increasing with each insert.
prop_severalSuccessiveInserts :: Int -> [String] -> Property
prop_severalSuccessiveInserts key values =
    not (null values) ==>
        query sn key == Just (last values)
        && head clocks < (head . tail) clocks
        && monotonicClocks
  where
    s0 :: AggregateState (ReplicatedKVStore Int String)
    s0     = initialize (P 1)
    ss     = scanl (flip (modify Add key)) s0 values
    sn     = last ss
    clocks = map clock ss

    monotonicClocks = and $ zipWith (<) clocks (tail clocks)

-- insert several arbitrary key-value pairs into a store, and then
-- ensure all keys can be queried
prop_insertsPersist :: [(Int, String)] -> Bool
prop_insertsPersist pairs = and queryResults
  where
    s0 :: AggregateState (ReplicatedKVStore Int String)
    s0 = initialize (P 1)
    sn = foldl' addToState s0 pairsWithUniqueKeys
      where
        addToState state (key, value) = modify Add key value state

    pairsWithUniqueKeys = nubBy (\p1 p2 -> fst p1 == fst p2) pairs

    uniqueKeys   = map fst pairsWithUniqueKeys
    queryResults = map keyValuePairInState pairsWithUniqueKeys
      where
        keyValuePairInState (key, value) = query sn key == Just value

-- Insert several arbitrary key-value pairs into a store, and then
-- remove them. None of the inserted values should be present
-- following the removes.
prop_valuesDontPersistAfterRemoves :: [(Int, String)] -> Property
prop_valuesDontPersistAfterRemoves pairs =
    not (null pairs) ==>
        allKeysAbsentAfterRemove
        && ci < cr
  where
    s0 :: AggregateState (ReplicatedKVStore Int String)
    s0   = initialize (P 1)

    si   = foldl' addToState s0 pairs
      where
        addToState state (key, value) = modify Add key value state
    ci   = clock si

    keys = map fst pairs
    sr   = foldl' removeFromState si keys
             where removeFromState state key = modify Remove key undefined state
    cr   = clock sr

    allKeysAbsentAfterRemove = all keyAbsentFromState keys
      where
        keyAbsentFromState key = isNothing (query sr key)

-- Compare behaviour of inserts and removes using a Map as a reference.
prop_equivalentToAnOrdinaryMap :: [(Int, String)] -> [Int] -> Bool
prop_equivalentToAnOrdinaryMap pairsToInsert keysToRemove =
    identicalToMapAfterInserts
    && identicalToMapAfterRemoves
  where
    s0 :: AggregateState (ReplicatedKVStore Int String)
    s0 = initialize (P 1)

    keys = map fst pairsToInsert

    reference :: Map.Map Int String
    reference = Map.empty

    si = foldl' addToState s0 pairsToInsert
      where
        addToState state (key, value) = modify Add key value state
    mi = Map.fromList pairsToInsert

    identicalToMap kvState mapState = all lookupsAreIdenticalFor keys
        where lookupsAreIdenticalFor key =
                  query kvState key == Map.lookup key mapState

    identicalToMapAfterInserts = identicalToMap si mi

    sr = foldl' removeFromState si keysToRemove
      where
        removeFromState state key = modify Remove key undefined state
    mr = foldl' (flip Map.delete) mi keysToRemove

    identicalToMapAfterRemoves = identicalToMap sr mr

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
