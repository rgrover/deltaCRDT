{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module CRDT.DeltaCvRDT where

import           CRDT.CvRDT

import           Algebra.Lattice (BoundedJoinSemiLattice,
                                  MeetSemiLattice)

-- A delta-interval-based, convergent replicated data types capable of
-- disseminating delta states (instead of complete clones) in order to
-- achieve eventual consistency.
--
--  s :: CRDT state forming a semilattice, where modifications are
--       inflationary and join is conflict-free.
--
-- In a δ-CRDT, the effect of applying a mutation, represented by a
-- delta-mutation δ = mδ(X), is decoupled from the resulting state
--
--     X′ = X ⊔ δ,
--
-- which allows shipping this δ rather than the entire resulting state X′.
class ( CvRDT s
      , Ord (VectorClock s) -- reflects causal ordering
      , BoundedJoinSemiLattice (VectorClock s)
      , MeetSemiLattice (VectorClock s)
      ) =>
      DeltaCvRDT s
    where

    -- An instance specific type for a monotonically increasing event
    -- counter. Every event which updates a state gets associated with
    -- a unique vector-clock.
    --
    -- i.   No two distinct events should get associated with the same
    -- clock.
    --
    -- ii.  Any total-order of events occuring at a replica should be
    -- reflected in the corresponding total order of their
    -- vector-clocks.
    --
    -- iii. Furthermore, it should be possible to compare causally
    -- related events occuring across replicas using the
    -- vector-clock--i.e.  clk(i) < clk(j) iff event(i) causally
    -- precedes event(j) regardless of which replica they occurred at.
    --
    -- Note: vectorClocks should *not* wrap around.
    type VectorClock s :: *

    -- fetch current clock from a given state
    clock :: s -> VectorClock s

    -- increment the replica-specific component of the state's vector-clock
    incrementClock :: s -> s

    -- A delta-mutator mδ is a function, corresponding  to  an  update
    -- operation,  which  takes  a  state X in  a  join-semilattice S
    -- as parameter and returns a delta-mutation mδ(X), also in S.
    --
    -- The state transition at each replica is given by either joining
    -- the current state X ∈ S with a delta-mutation: X′ = X ⊔ mδ(X)
    -- or joining the current state with some received delta-group D:
    --     X′ = X ⊔ D.
    --
    -- Furthermore, if m is the corresponding state modifier of a
    -- CvRDT, then
    --     m(X) = X ⊔ mδ(X)
    --
    -- Note: a δ is just a state, that can be joined possibly several
    -- times without requiring exactly-once delivery, and without
    -- being a representation of the “increment” operation (as in
    -- operation-based CRDTs), which is itself non-idempotent;
    deltaMutation :: OpsType s -> KeyType s -> ValueType s -> s -> s
