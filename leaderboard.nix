{ mkDerivation, aeson, base, beam-core, beam-migrate, beam-postgres
, bytestring, connection, cryptonite, hedgehog, http-client-tls
, http-types, jose, lens, mmorph, monad-control, monad-log, mtl
, optparse-applicative, postgresql-simple, postgresql-simple-url
, product-profunctors, resource-pool, retry, scrypt, servant
, servant-auth, servant-auth-client, servant-auth-server
, servant-client, servant-server, stdenv, tasty, tasty-hedgehog
, text, tmp-postgres, transformers, transformers-base
, uri-bytestring, warp, warp-tls
}:
mkDerivation {
  pname = "leaderboard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base beam-core beam-migrate beam-postgres bytestring
    cryptonite http-client-tls jose lens monad-control monad-log mtl
    optparse-applicative postgresql-simple product-profunctors
    resource-pool retry scrypt servant-auth servant-auth-server
    servant-server text transformers-base uri-bytestring warp warp-tls
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    base bytestring connection hedgehog http-client-tls http-types lens
    mmorph postgresql-simple postgresql-simple-url servant
    servant-auth-client servant-client tasty tasty-hedgehog text
    tmp-postgres transformers
  ];
  license = stdenv.lib.licenses.bsd3;
}
