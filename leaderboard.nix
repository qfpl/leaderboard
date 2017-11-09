{ mkDerivation, aeson, base, beam-core, beam-migrate, beam-postgres
, bytestring, hedgehog, hoauth2, http-client-tls, lens, monad-log
, mtl, optparse-applicative, postgresql-simple, product-profunctors
, retry, servant-server, stdenv, text, uri-bytestring, warp
}:
mkDerivation {
  pname = "leaderboard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beam-core beam-migrate beam-postgres hoauth2
    http-client-tls lens monad-log mtl postgresql-simple
    product-profunctors servant-server text uri-bytestring
  ];
  executableHaskellDepends = [
    base bytestring hoauth2 monad-log optparse-applicative
    postgresql-simple retry text uri-bytestring warp
  ];
  testHaskellDepends = [ base hedgehog ];
  license = stdenv.lib.licenses.bsd3;
}
