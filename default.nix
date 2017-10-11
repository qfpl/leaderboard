{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let

  haskellPackages =
    if compiler == "default"
    then nixpkgs.haskell.packages.ghc821
    else nixpkgs.haskell.packages.${compiler};
in
haskellPackages.callPackage ./leaderboard.nix { }
