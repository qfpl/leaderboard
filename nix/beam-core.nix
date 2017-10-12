{ haskellPackages, beam }:
haskellPackages.callCabal2nix "beam-core" beam.core {}
