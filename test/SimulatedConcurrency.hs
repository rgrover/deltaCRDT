{-# LANGUAGE TemplateHaskell #-}
module SimulatedConcurrency (runTests) where

import           CRDT.CRDT

import           Impl.KVStore.Pid
import           Impl.KVStore.ReplicatedKVStore (KVStoreOps (..), ReplicatedKVStore (..))

import           Data.List                      (foldl')
import qualified Data.Map                       as Map (Map (..),
                                                        fromList,
                                                        insert,
                                                        toList, (!))
import           Data.Maybe                     (fromJust)
import           Debug.Trace                    (trace)

import           Test.QuickCheck

numReplicas = 5

data Action
    = Insert
        Pid    -- target replicaId
        Int    -- key
        String -- value
    | Delete
        Pid    -- target replicaId
        Int    -- key
    | Message
        Pid    -- sender
        Pid    -- receiver
    | Ack
        Pid    -- sender
        Pid    -- receiver
    deriving (Show)

instance Arbitrary Action where
    arbitrary = do
        let
            keyGen = choose (1, 31) -- say, days in a month
            valueGen =  -- say, summary of weather report
                elements
                    [ ""
                    , "Sunny"
                    , "Heat wave"
                    , "Calm"
                    , "Foggy"
                    , "Cloudy"
                    , "Drizzling"
                    , "Hail"
                    , "Freezing"
                    ]
            replicaGen = P <$> choose (0, (numReplicas - 1))
            insert = do
                replica <- replicaGen
                key <- keyGen
                value <- valueGen
                return (Insert replica key value)
            delete = do
                replica <- replicaGen
                key <- keyGen
                return (Delete replica key)
            messageTo = do
                sender <- replicaGen
                receiver <- replicaGen
                return (Message sender receiver)
            ackTo = do
                sender <- replicaGen
                receiver <- replicaGen
                return (Ack sender receiver)
        oneof [insert, delete, messageTo, ackTo]

type Replica = AggregateState (ReplicatedKVStore Int String)

applyAction :: Map.Map Pid Replica -> Action -> Map.Map Pid Replica
applyAction stateMap action =
    case action of
        Insert replica key value ->
            let s  = stateMap Map.! replica
                s' = modify Add key value s
            in Map.insert replica s' stateMap
        Delete replica key ->
            let s  = stateMap Map.! replica
                s' = modify Remove key undefined s
            in Map.insert replica s' stateMap
        Message sender receiver ->
            let s1  = stateMap Map.! sender
                s2  = stateMap Map.! receiver
                m12 = composeMessageTo receiver s1
                s2' = onReceive m12 s2
            in Map.insert receiver s2' stateMap
        Ack sender receiver ->
            let s1  = stateMap Map.! sender
                m12 = composeAckMessage s1
                s2  = stateMap Map.! receiver
                s2' = onReceive m12 s2
            in Map.insert receiver s2' stateMap

-- Apply arbitrary actions to a small collection of states. These
-- actions do include sending messages and acknowledgements, so there
-- is synchronization to some degree. Following the application of
-- actions, explicitly send delta messages (if necessary) between all
-- replicas to achieve eventual consistency.
--
--   * test that all replicas have consistent values for all keys. We've
--     deliberately chosen keys from a small set to simplify validation.
prop_arbitraryOperationsSimulatingConcurrency =
    forAll (scale (* 1) (arbitrary :: Gen [Action])) $ \actions ->
        not (null actions) ==>
        let
            pids = P <$> [0 .. (numReplicas - 1)]

            -- original states
            states :: Map.Map Pid Replica
            states = Map.fromList $ zip pids (initialize <$> pids)

            -- states after applying all actions
            states' = foldl' applyAction states actions

            -- states after applying brute-force synchronization of
            -- deltas at the end.
            syncs :: [Action]
            syncs = [Message s r | s <- pids, r <- pids, s /= r]
            --syncs = [Message s r | s <- (P <$> [0..2]), r <- pids, s /= r]
            states'' = foldl' applyAction states' syncs

            statesAsList :: [Replica]
            statesAsList = map snd $ Map.toList states''

            valuesMatchFor key =
                and $ zipWith (==) values (tail values)
              where
                values = (flip query key) <$> statesAsList
        in all valuesMatchFor [1 .. 31]
        --in trace (show (statesAsList !! 0)) $ False

--------------------------
return []
runTests :: IO Bool
runTests = $quickCheckAll
