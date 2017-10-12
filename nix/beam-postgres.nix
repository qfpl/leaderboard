{ haskellPackages, beam }:
haskellPackages.callCabal2nix "beam-postgres" beam.postgres {}
