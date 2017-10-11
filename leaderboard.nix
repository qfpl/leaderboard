{ mkDerivation, aeson, base, beam-core, beam-postgres, lens
, opaleye, postgresql-simple, product-profunctors, servant-server
, stdenv, text, warp
}:
mkDerivation {
  pname = "leaderboard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beam-core beam-postgres lens opaleye postgresql-simple
    product-profunctors servant-server text
  ];
  executableHaskellDepends = [ base warp ];
  license = stdenv.lib.licenses.bsd3;
}
