{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  haskellPackages =
    import ./nix/haskellPackagesWithBeam.nix { inherit nixpkgs compiler; };

in
nixpkgs.haskell.lib.justStaticExecutables
  (haskellPackages.callPackage
    ./leaderboard.nix
    {})
