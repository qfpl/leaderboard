{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  haskellPackages =
    import ./nix/modifiedHaskellPackages.nix { inherit nixpkgs compiler; };
in
nixpkgs.haskell.lib.justStaticExecutables
  (haskellPackages.callPackage
    ./leaderboard.nix
    { })
