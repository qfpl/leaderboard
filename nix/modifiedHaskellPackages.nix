{ addToBuildDepends, nixpkgs, compiler }:
let
  beam = import ./beam.nix { inherit nixpkgs; };
  tmp-postgres-build-deps = with nixpkgs; [postgresql procps];
in
  nixpkgs.haskell.packages.${compiler}.override {
  overrides = self: super: {
    beam-core =
      import ./beam-core.nix { haskellPackages = self; inherit beam; };
    beam-postgres =
      import ./beam-postgres.nix { haskellPackages = self; inherit beam; };
    beam-migrate =
      import ./beam-migrate.nix { haskellPackages = self; inherit beam; };
    monad-log = self.callPackage ./monad-log.nix {};
    # servant-auth-client in nixpkgs (17.09 and 18.03 at least) is too new for servant-0.11
    servant-auth-client =
      # Tests fail on GHC 8.2.2 due to main module not being named "Main.hs"
      nixpkgs.haskell.lib.dontCheck (self.callHackage "servant-auth-client" "0.3.0.0" {});
    tmp-postgres = addToBuildDepends (self.callPackage ./tmp-postgres.nix {}) tmp-postgres-build-deps;
    # concurrent-output depends on process >= 1.6, and GHC 8.0.2 comes with 1.4
    concurrent-output =
      if nixpkgs.haskell.compiler.${compiler}.version < "8.2"
      then nixpkgs.haskell.lib.doJailbreak super.concurrent-output
      else super.concurrent-output;
  };
}
