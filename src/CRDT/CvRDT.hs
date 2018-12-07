{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module CRDT.CvRDT where

import           Misc.Pid

import           Algebra.Lattice (BoundedJoinSemiLattice, (\/))

-- Convergent replicated data types (eventually consistent based on
-- state).
--
-- State-based CRDTs demand fewer guarantees from the dissemination
-- layer, working under message loss, duplication, reordering, and
-- temporary network partitioning, without impacting availability and
-- eventual convergence
--
-- The state forms a join semilattice, which means that any finite
-- collection of states can be joined to form a least-upper-bound.
class (BoundedJoinSemiLattice s, Eq s) => CvRDT s where
    type Ops s       :: * -- Enumeration type for permitted operations
    type KeyType s   :: *
    type ValueType s :: *

    -- local fetch based on current state
    query :: s -> KeyType s -> ValueType s

    -- The modify function. This must monotonically increase the
    -- internal state, according to the same partial order rules as
    -- the semilattice.  This automatically resolves conflicts when
    -- merging states and results in strong eventual consistency.
    --
    -- If X ∈ s and m is a modify operation, then X ⊑ m(X), where ⊑ is
    -- the `joinLeq`--i.e. the partial ordering induced by the
    -- semilattice.
    --
    -- Note: Pid parameter is used for passing own process Id
    modify :: Pid -> Ops s -> KeyType s -> ValueType s -> s -> s

    -- The merge function provides a join for any pair of replica
    -- states. It needs to be commutative, associative, and
    -- idempotent.
    merge :: s -> s -> s
    merge = (\/)
