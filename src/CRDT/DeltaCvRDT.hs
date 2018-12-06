{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}

module CRDT.DeltaCvRDT
    ( DeltaCvRDT
    , initDeltaCvRDTState
    , DeltaCvRDTState
    ) where

import           CRDT.CvRDT
import           Misc.Pid
import           Misc.VectorClock

import           Algebra.Lattice                  (BoundedJoinSemiLattice,
                                                   bottom)
import qualified Data.Map.Strict                  as Map
import qualified Data.Sequence                    as Seq

import           Control.Monad.Trans.State.Strict as S

-- A delta-interval-based, convergent replicated data types capable of
-- disseminating delta states (instead of complete clones) in order to achieve
-- eventual consistency.
--
--  s :: CRDT state forming a semilattice, where modifications are
--          inflationary and join is conflict-free.
--  p :: identifier for the local process
--  o :: identifier for operations permitted on state
--  k :: key--i.e. an identifier for some element within state
--  v :: result of querying state with a key
--
-- In a δ-CRDT, the effect of applying a mutation, represented by a
-- delta-mutation δ = mδ(X), is decoupled from the resulting state X′ = X ⊔ δ,
-- which allows shipping this δ rather than the entire resulting state X′
class CvRDT s p o k v =>
      DeltaCvRDT s p o k v
    | s -> p o k v
    where
    -- A delta-mutator mδ is a function, corresponding  to  an  update
    -- operation,  which  takes  a  state X in  a  join-semilattice S as
    -- parameter and returns a delta-mutation mδ(X), also in S.
    --
    -- The state transition at each replica is given by either joining the
    -- current state X ∈ S with a delta-mutation:
    --     X′ = X ⊔ mδ(X)
    -- or joining the current state with some received delta-group D:
    --     X′ = X ⊔ D.
    --
    -- Furthermore, if m is the corresponding state modifier of a CvRDT, then
    --     m(X) = X ⊔ mδ(X)
    --
    -- Note: a δ is just a state, that can be joined possibly several times
    -- without requiring exactly-once delivery, and without being a
    -- representation of the “increment” operation (as in operation-based
    -- CRDTs), which is itself non-idempotent;
    deltaMutation :: p -> o -> k -> v -> s -> s

-- A sequence of deltas tagged with vector-clocks. This is used to exchange deltas
-- between processes, and also to maintain a local copy of deltas waiting to be
-- disseminated.
-- Note: Delta-Intervals may be held in volatile storage.
data DeltaInterval s where
    DeltaInterval
        :: BoundedJoinSemiLattice s
        => Seq.Seq (VectorClock, s)
        -> DeltaInterval s

-- Each process i keeps an acknowledgment map Ai that stores, for each
-- neighbor j, the largest clock b for all delta-intervals acknowledged
-- by j. Ai[i] should match a process's own vector clock.
-- Note: this map may be held in volatile storage.
type AcknowledgementMap = Map.Map Pid VectorClock

-- Application facing state
data DeltaCvRDTState s where
    DeltaCvRDTState
        :: DeltaCvRDT s p o k v
        => ( s                  -- main CRDT state
           , VectorClock        -- local vector-clock
           , DeltaInterval s    -- delta-group pending dissemination
           , AcknowledgementMap -- knowledge of neighbour state
           )
        -> DeltaCvRDTState s

initDeltaCvRDTState :: DeltaCvRDT s p o k v => DeltaCvRDTState s
initDeltaCvRDTState =
    DeltaCvRDTState (bottom, bottom, DeltaInterval Seq.empty, Map.empty)

data Message
    = Delta
    | Ack

