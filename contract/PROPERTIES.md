# Properties

Goal is that any honest person will always be able to reject bad commits and able to defend good commits.

## Game Properties

All games must always a contain a left hash that both players agree on and a right hash that both players disagree on.

All hashes are committer's hashes.

All intial state hash must not be halted.

All mid state hash must not be halted.

All final state hash must be halted.

All final state hash have a non zero step.

The game should always start or converge to a state where the initial state hash step is one smaller than the final state hash.

## Vm Property

All state hashes must be probabilitistic unique.

Must have a total ordering over all the states.
