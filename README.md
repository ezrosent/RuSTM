# RuSTM
software transactional memory for rust-lang

# Approach
We aim to implement a version of the NoRec STM in Rust, while providing
Hakell STM-like semantics. Differences from NoRec include:
    * explicit `TVar`s to hold transactional state
    * composition with `or_else`
    * No use of longjump as in the original NoRec paper, instead use of
      `Result`s and explicit retries
    * Allows for the Haskell convention of a thread sleeping after an aborted
      transaction until an object in its readset changes.

Our general hope is to show that this can be implemented in Rust in a fairly
straight-forward manner, only breaking the rules in limited and understandable
ways.

# Status
Still in the initial implementation stage.
