{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let

  inherit (nixpkgs) pkgs;

  haskellPackages =
    import ./nix/modifiedHaskellPackages.nix { inherit nixpkgs compiler; };

  drv =
    pkgs.haskell.lib.overrideCabal
      (haskellPackages.callPackage ./leaderboard.nix {})
      (drv: {
        buildDepends = (drv.buildDepends or []) ++
          [ (haskellPackages.hoogleLocal {
              packages =
                drv.libraryHaskellDepends ++
                drv.executableHaskellDepends;
              })
          ];
      });

in
  if pkgs.lib.inNixShell then drv.env else drv
