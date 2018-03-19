{ mkDerivation, aeson, base, beam-core, beam-migrate, beam-postgres
, bytestring, hedgehog, http-client-tls, jose, lens, monad-control
, monad-log, mtl, optparse-applicative, postgresql-simple
, product-profunctors, resource-pool, retry, servant-auth
, servant-auth-server, servant-server, stdenv, text
, transformers-base, uri-bytestring, warp
}:
mkDerivation {
  pname = "leaderboard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beam-core beam-migrate beam-postgres bytestring
    http-client-tls jose lens monad-control monad-log mtl
    postgresql-simple product-profunctors resource-pool servant-auth
    servant-auth-server servant-server text transformers-base
    uri-bytestring
  ];
  executableHaskellDepends = [
    base bytestring monad-log optparse-applicative postgresql-simple
    retry text uri-bytestring warp
  ];
  testHaskellDepends = [ base hedgehog ];
  license = stdenv.lib.licenses.bsd3;
}
