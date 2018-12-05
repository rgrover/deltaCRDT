{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}

module CRDT.DeltaCvRDT where

import           CRDT.CvRDT
import           Data.Sequence

import           Algebra.Lattice (BoundedJoinSemiLattice)

-- Convergent replicated data types capable of disseminating delta states
-- (instead of complete clones) in order to achieve eventual consistency.
--
--  s :: state forming a semilattice, possibly using a clock capturing causal dependency
--  p :: identifier for the local process
--  o :: identifier for operations permitted on state
--  k :: key--i.e. an identifier for some element within state
--  v :: result of querying state with a key
class CvRDT s p o k v => DeltaCvRDT s p o k v
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

-- In a δ-CRDT, the effect of applying a mutation, represented by a
-- delta-mutation δ = mδ(X), is decoupled from the resulting state X′ = X ⊔ δ,
-- which allows shipping this δ rather than the entire resulting state X′
type DeltaGroup a = BoundedJoinSemiLattice a => Seq a
