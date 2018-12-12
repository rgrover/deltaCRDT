# delta-crdt

This is a library for modeling replicated, converging, eventually consistent,
and conflict-free data types for the purpose of keeping distributed state.

In particular, it allows modeling of δ-CvRDTs, which are a variant on CRDTs,
which permit expression and dissemination of state changes as lightweight delta
mutations.

The library specifies mathematical properties and data-types expected of a CRDT
implementation. Given a satisfactory implementation for the underlying
mathematical concepts and types, generic algorithms become available to a user
to safely mutate replica states and collect state changes meant for
dissemination across replicas.

A replicated data-type allows multiple independent data-stores to exist
concurrently. Replicas are kept synchronized by exchanging messages between
them. Messages contain information about state transitions, and the implementor
is expected to periodically generate such messages and disseminate them between
replicas in a manner that ensures eventual consistency. Using a δ-CvRDT results
in generation of shorter messages containing mostly state transitions.

Eventual consistency relies on a message transport layer to exchange
information about state transitions between the replicas. This library does not
require any particular transport layer.  It assumes that messages can be
exchanged without getting damaged. Messages can be lost.

A Replicated-KVStore (i.e. an Observed-Remove set using 'add-wins' policy to
resolve conflicts) is provided initially as a reference implementation for the
underlying concepts. There is a test suite which exercises multiple replicas in
a manner which emulates concurrent updates.

# References

https://en.wikipedia.org/wiki/Conflict-free_replicated_data_type

Efficient State-based CRDTs by Delta-Mutation (https://arxiv.org/pdf/1410.2803.pdf)
