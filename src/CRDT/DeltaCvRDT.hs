{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}

module CRDT.DeltaCvRDT where

import           CRDT.CvRDT
import           Misc.Pid
import           Misc.VectorClock

import           Algebra.Lattice                  (BoundedJoinSemiLattice)
import           Data.Map.Strict                  as Map
import           Data.Sequence

import           Control.Monad.Trans.State.Strict as S

-- A delta-interval-based, convergent replicated data types capable of
-- disseminating delta states (instead of complete clones) in order to achieve
-- eventual consistency.
--
--  s :: state forming a semilattice, possibly using a clock capturing causal dependency
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

-- A sequence of deltas with clocks. This is used to exchange deltas between
-- processes, and also to maintain a local copy of deltas waiting to be
-- disseminated.
-- Note: Delta-groups are volatile.
data DeltaInterval s where
    DeltaInterval
        :: BoundedJoinSemiLattice s => Seq (VectorClock, s) -> DeltaInterval s

-- Each process i keeps an acknowledgment map Ai that stores, for each
-- neighbor j, the largest clock b for all delta-intervals acknowledged
-- by j. Ai[i] should match a process's own vector clock.
--
-- Note: this is volatile.
type AcknowledgementMap = Map Pid VectorClock

type DeltaCvRDTState s
     = S.State ( s
               , VectorClock
               , DeltaInterval s    -- delta-group pending dissemination
               , AcknowledgementMap -- knowledge of neighbour state
               ) -- Note: for fault tolerance, the first two members of
                 -- this pair need to be persistent.
