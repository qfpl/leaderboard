{ mkDerivation, aeson, base, beam-core, beam-migrate, beam-postgres
, bytestring, lens, mtl, opaleye, optparse-applicative
, postgresql-simple, product-profunctors, retry, servant-server
, stdenv, text, warp
}:
mkDerivation {
  pname = "leaderboard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beam-core beam-migrate beam-postgres lens mtl opaleye
    postgresql-simple product-profunctors servant-server text
  ];
  executableHaskellDepends = [
    base beam-migrate beam-postgres bytestring optparse-applicative
    postgresql-simple retry warp
  ];
  license = stdenv.lib.licenses.bsd3;
}
