{ haskellPackages, beam }:
haskellPackages.callPackage
  (import "${beam.migrate}/beam-migrate.nix")
  {}
