{ nixpkgs ? "default", compiler ? "ghc802" }:
let
  env = import ./nix/env.nix {inherit nixpkgs compiler; };
  drv =
    env.pkgs.haskell.lib.overrideCabal
      env.leaderboard
      (drv: {
        buildDepends = (drv.buildDepends or []) ++
          [ (env.haskellPackages.hoogleLocal {
              packages =
                drv.libraryHaskellDepends ++
                drv.executableHaskellDepends ++
                drv.testHaskellDepends;
              })
          ] ++
          (with env.haskellPackages; [ghcid cabal-install]);
      });
in
  if env.pkgs.lib.inNixShell then drv.env else drv
