{ mkDerivation, base, selda, servant, stdenv }:
mkDerivation {
  pname = "leaderboard";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [ base selda servant ];
  license = stdenv.lib.licenses.bsd3;
}
