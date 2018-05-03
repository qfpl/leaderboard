{ nixpkgs ? "default", compiler ? "ghc802" }:
let
  env = import ./nix/env.nix { inherit nixpkgs compiler; };
  leaderboard =
    env.pkgs.haskell.lib.overrideCabal
      env.leaderboard
      (old: {
        buildDepends = (old.buildDepends or []) ++ [ env.pkgs.postgresql ]; });
in
  env.pkgs.haskell.lib.justStaticExecutables leaderboard
