module CRDT.Core.Message where

import           CRDT.Core.AggregateState (DeltaInterval)
import           CRDT.Core.CvRDT          (ReplicaId)
import           CRDT.Core.DeltaCvRDT     (VectorClock)

import           Data.Map.Strict          as Map (Map)
import           Data.Sequence            as Seq (Seq)

-- A message between two processes can either hold Deltas along with
-- clocks--i.e. DeltaInterval--or it can be an acknowledgement for
-- previously sent deltas
data Message s
    = Ack (ReplicaId s)        -- sender's id
          (VectorClock s)      -- sender's clock
    | Deltas (ReplicaId s)     -- sender's id
             (VectorClock s)   -- sender's clock
             (DeltaInterval s) -- deltas
