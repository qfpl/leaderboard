{ haskellPackages, beam }:
  haskellPackages.callPackage
    (import "${beam.postgres}/beam-postgres.nix")
    {}
