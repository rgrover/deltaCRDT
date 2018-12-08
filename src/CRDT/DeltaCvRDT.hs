{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
module CRDT.DeltaCvRDT where

import           CRDT.CvRDT

import           Algebra.Lattice (BoundedJoinSemiLattice)

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
      , Num (EventCounter s)
      , Ord (EventCounter s)
      ) =>
      DeltaCvRDT s
    where

    type EventCounter s :: * -- Monotonically increasing event counter
                             --    local to a process. Could be any
                             --    numeric type supporting increment.
                             --    Note: The user is responsible for
                             --    ensuring that there is no wraparound.

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
    deltaMutation :: Ops s -> KeyType s -> ValueType s -> s -> s
