# Initial Investigation adding SMT Dependencies to Plutus Libraries

Running tests 

1. `nix develop`
2. `cd plutus-tx`
3. `cabal run liquid-test`

Note that these tests don't test the actual modules in the library themselves but instead run some basic smt tests. Proper library testing is left to future work.
