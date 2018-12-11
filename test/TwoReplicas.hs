{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
module TwoReplicas (runTests) where

import           CRDT.Core.Message              as Msg
import           CRDT.CRDT

import           Impl.KVStore.Pid
import           Impl.KVStore.ReplicatedKVStore (KVStoreOps (..), ReplicatedKVStore (..))

import           Algebra.Lattice                (bottom, (\/))

import           Data.List                      (foldl', nubBy,
                                                 zipWith)
import qualified Data.Map                       as Map
import           Data.Maybe                     (fromJust, isNothing)

import           Debug.Trace                    (trace)
import           Test.QuickCheck

-- initialize two replicas with arbitrary Pids
--   * check replica's pids
--   * check that there is no causal dependency between the two clocks
prop_initializeTwoReplicas :: Word -> Word -> Property
prop_initializeTwoReplicas pid1 pid2 =
    pid1 /= pid2 ==>
        pid s1 == P pid1
        && pid s2 == P pid2
        && clock s1 `compare` clock s2 == EQ
  where
    s1 :: AggregateState (ReplicatedKVStore Int String)
    s1 = initialize (P pid1)
    s2 :: AggregateState (ReplicatedKVStore Int String)
    s2 = initialize (P pid2)

-- initialize replica1 and replica2; and insert an arbitrary k-v pair
-- into both
--   * lookup should succeed for either replica
--   * replica's clocks should not be causally related
prop_singleConcurrentInsert :: Int -> String -> Bool
prop_singleConcurrentInsert key value =
    query s1' key == Just value
    && query s2' key == Just value
    && not (clock s1' < clock s2')
    && not (clock s2' < clock s1')
    && (clock s1' `compare` clock s2' == EQ)
    -- && isNothing (query state' key') -- lookup of a different key
    -- && bottom < clock state'
  where
    s1 :: AggregateState (ReplicatedKVStore Int String)
    s1  = initialize (P 1)
    s2 :: AggregateState (ReplicatedKVStore Int String)
    s2  = initialize (P 2)
    s1' = modify Add key value s1
    s2' = modify Add key value s2

-- insert a separate arbitrary k-v pair into each replica, and then
-- remove it
--   * lookup following the remove should yield Nothing
--   * replica's clocks (after insert and remove) should remain
--     mutually concurrent
prop_separateInsertsFollowedByRemoves ::
       Int -> String -> Int -> String -> Bool
prop_separateInsertsFollowedByRemoves k1 v1 k2 v2 =
    isNothing (query s1'' k1)
    && isNothing (query s2'' k2)
    && (c1' `compare` c2' == EQ)
    && (c1'' `compare` c2'' == EQ)
  where
    s1 :: AggregateState (ReplicatedKVStore Int String)
    s1   = initialize (P 1)
    s2 :: AggregateState (ReplicatedKVStore Int String)
    s2   = initialize (P 2)
    s1'  = modify Add k1 v1 s1
    c1'  = clock s1'
    s2'  = modify Add k2 v2 s2
    c2'  = clock s2'
    s1'' = modify Remove k1 undefined s1'
    c1'' = clock s1''
    s2'' = modify Remove k2 undefined s2'
    c2'' = clock s2''

-- insert an arbitrary k-v pair into a replica, and then
-- send a message from this replica to the other.
--   * the message should be non-empty
--   * the message should contain the full state of s1
--   * lookup of key in the other replica should succeed
--   * clock of the second replica should be > its initial clock
--   * clock of the second replica should be > the clock of the first
--   * lookup of the key in the first replica should still succeed
--
--   * replica2 composes an empty message for replica1 after the exchange
prop_singleInsertFollowedByMessageExchangeGivesConsistency ::
       Int -> String -> Bool
prop_singleInsertFollowedByMessageExchangeGivesConsistency key value =
    c1' `compare` c2 == EQ
    && not (isNothing m12)
    && msgIsFullState
    && (query s2' key) == Just value
    && c1' < c2'
    && c2 < c2'
    && c1' \/ c2 < c2'
    && pid s2' == P 2
    && isNothing m21
  where
    s1 :: AggregateState (ReplicatedKVStore Int String)
    s1 = initialize (P 1)
    s2 :: AggregateState (ReplicatedKVStore Int String)
    s2  = initialize (P 2)
    c2  = clock s2
    s1' = modify Add key value s1
    c1' = clock s1'
    m12 = composeDeltasMessageTo (P 2) s1'

    msgIsFullState =
        case m12 of
            Nothing -> False
            Just msg ->
                case msg of
                    State _   -> True
                    otherwise -> False

    s2' = onReceive (fromJust m12) s2
    c2' = clock s2'

    m21 = composeDeltasMessageTo (P 1) s2'

-- insert an arbitrary k-v pair into each replica, and then
-- send a messages between the two replicas
--   * both messages should be non-empty
--   * lookup of either key in either replica should succeed
--   * clock of the second replica should be > its initial clock
--   * clock of the second replica should be > the clock of the first
--   * replica1 composes an empty message for replica2 after the exchange
prop_concurrentInsertsFollowedByMessageExchangesGiveConsistency ::
       Int -> String -> Int -> String -> Property
prop_concurrentInsertsFollowedByMessageExchangesGiveConsistency k1 v1 k2 v2 =
    k1 /= k2 ==>
        (query s2'' k1) == Just v1
        && (query s1'' k1) == Just v1
        && (query s1'' k2) == Just v2
        && (query s2'' k2) == Just v2
        && clock s1' < clock s1''
        && clock s2'' < clock s1''
        && pid s1'' == P 1
        && isNothing (m12')
  where
    s1 :: AggregateState (ReplicatedKVStore Int String)
    s1 = initialize (P 1)
    s2 :: AggregateState (ReplicatedKVStore Int String)
    s2  = initialize (P 2)
    s1' = modify Add k1 v1 s1
    c1' = clock s1'
    s2' = modify Add k2 v2 s2
    c2' = clock s2'

    m12  = composeDeltasMessageTo (P 2) s1'
    s2'' = onReceive (fromJust m12) s2'

    m21  = composeDeltasMessageTo (P 1) s2''
    s1'' = onReceive (fromJust m21) s1'

    m12' = composeDeltasMessageTo (P 2) s1''

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
