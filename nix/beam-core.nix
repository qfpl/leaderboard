{ haskellPackages, beam }:
haskellPackages.callPackage
  (import "${beam.core}/beam-core.nix")
  {}
