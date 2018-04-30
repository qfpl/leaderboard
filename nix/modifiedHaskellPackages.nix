{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
let
  beam = import ./beam.nix { inherit nixpkgs; };
  monad-log = import ./monad-log.nix;
in
(if compiler == "default"
then nixpkgs.haskell.packages.ghc802
else nixpkgs.haskell.packages.${compiler}).override {
  overrides = self: super: {
    beam-core =
      import ./beam-core.nix { haskellPackages = self; inherit beam; };
    beam-postgres =
      import ./beam-postgres.nix { haskellPackages = self; inherit beam; };
    beam-migrate =
      import ./beam-migrate.nix { haskellPackages = self; inherit beam; };
    monad-log = self.callPackage monad-log {};
    # servant-auth-client is too new on 17.09
    servant-auth-client =
      if self.servant.version < "0.13"
      then self.callPackage ./servant-auth-client-0.3.nix {}
      else self.servant-auth-client;
  };
}
