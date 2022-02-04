# Terminology

- state_hash: hash of the entire vm e.g. Vm.hash
- input_hash: hash of the input being processed by a vm
- storage_hash: hash of only the storage of a vm
- input_storage_hash: hash of input and storage combined

- initial_state_hash: hash of the step 0 of the vm
- final_state_hash: hash of the halted(last step) vm

- initial_input_hash: hash of all the input processed in a level
- final_input_hash: hash of the input of a halted vm

- previous_storage_hash: hash of the storage of the previous level's halted vm

TODO: points to the protocol here

- mid state hash: a state hash of a non halted vm used as the mid of a game
- halted state hash: a state hash of a vm that is halted
