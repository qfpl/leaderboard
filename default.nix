{ nixpkgs ? "default", compiler ? "ghc802" }:
let
  env = import ./nix/env.nix { inherit nixpkgs compiler; };
  leaderboard = env.addToBuildDepends env.leaderboard [env.pkgs.postgresql];
in
  env.pkgs.haskell.lib.justStaticExecutables leaderboard
