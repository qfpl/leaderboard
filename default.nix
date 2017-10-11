{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  haskellPackages =
    if compiler == "default"
    then nixpkgs.haskell.packages.ghc821
    else nixpkgs.haskell.packages.${compiler};

  beam =
    nixpkgs.callPackage
    ({ fetchFromGitHub, cabal2nix }: 
    nixpkgs.stdenv.mkDerivation {
        name = "beam";
        src = fetchFromGitHub {
          owner = "tathougies";
          repo = "beam";
          rev = "2ebec0f31dc53eb63f4298ae2e9d74bdbce20d28";
          sha256 = "02z4fsrwiawy2zcgb97vfdhsw5pw0nj4jhgpyx2hpasl5isjvgzm";
        };
        buildInputs = [ cabal2nix ];
        outputs =
          [ "out"
            "core"
            "postgres"
            "migrate"
            "sqlite"
          ];
        buildPhase = ''
          (cd beam-core && cabal2nix . > ./beam-core.nix)
          (cd beam-postgres && cabal2nix . > ./beam-postgres.nix)
          (cd beam-migrate && cabal2nix . > ./beam-migrate.nix)
          (cd beam-sqlite && cabal2nix . > ./beam-sqlite.nix)
        '';
        installPhase = ''
          mkdir -p $core
          mkdir -p $postgres
          mkdir -p $migrate
          mkdir -p $sqlite
          mkdir -p $out

          cp -R ./beam-core/* $core
          cp -R ./beam-postgres/* $postgres
          cp -R ./beam-migrate/* $migrate
          cp -R ./beam-sqlite/* $sqlite
        '';
      })
      {};
  beam-core =
    haskellPackages.callPackage
      (import "${beam.core}/beam-core.nix")
      {};
  beam-migrate =
    haskellPackages.callPackage
      (import "${beam.migrate}/beam-migrate.nix")
      { inherit beam-core; };
  beam-postgres =
    haskellPackages.callPackage
      (import "${beam.postgres}/beam-postgres.nix")
      { inherit beam-core beam-migrate; };
in
# nixpkgs.haskell.lib.justStaticExecutables
  (haskellPackages.callPackage
    ./leaderboard.nix
    { inherit beam-core beam-postgres; })
