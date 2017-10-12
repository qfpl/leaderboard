{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.callPackage
  ({ fetchFromGitHub }: 
  nixpkgs.stdenv.mkDerivation {
      name = "beam";
      src = fetchFromGitHub {
        owner = "lightandlight";
        repo = "beam";
        rev = "0b68b591068d33fd1a2fee6a4871eb6ab4117496";
        sha256 = "0pz20i710j3imbrxfiksy724d63277qd0nhqik9r4gxngm9as4d2";
      };
      outputs =
        [ "out"
          "core"
          "postgres"
          "migrate"
          "sqlite"
        ];
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
    {}
