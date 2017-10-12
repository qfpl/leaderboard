{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  beam = import ./beam.nix { inherit nixpkgs; };
in
(if compiler == "default"
then nixpkgs.haskell.packages.ghc821
else nixpkgs.haskell.packages.${compiler}).override {
  overrides = self: super: {
    beam-core =
      import ./beam-core.nix { haskellPackages = self; inherit beam; };
    beam-postgres =
      import ./beam-postgres.nix { haskellPackages = self; inherit beam; };
    beam-migrate =
      import ./beam-migrate.nix { haskellPackages = self; inherit beam; };
  };
}
