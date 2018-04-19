{ nixpkgs ? import <nixpkgs> {} }:
nixpkgs.callPackage
  ({ fetchFromGitHub }:
  nixpkgs.stdenv.mkDerivation {
      name = "beam";
      src = fetchFromGitHub {
        owner = "tathougies";
        repo = "beam";
        rev = "ba7c05a743d1924588a436d34c741a949adb2cdd";
        sha256 = "0dhdxnb3k9z6mcsaqk78fpj3p6s8ndjmr6s8i00bwbv6g2s8dxs5";
      };
      outputs =
        [ "out"
          "core"
          "postgres"
          "migrate"
        ];
      installPhase = ''
        mkdir -p $core
        mkdir -p $postgres
        mkdir -p $migrate
        mkdir -p $out

        cp -R ./beam-core/* $core
        cp -R ./beam-postgres/* $postgres
        cp -R ./beam-migrate/* $migrate
      '';
    })
    {}
