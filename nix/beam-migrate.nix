{ haskellPackages, beam }:
haskellPackages.callCabal2nix "beam-migrate" beam.migrate {}
