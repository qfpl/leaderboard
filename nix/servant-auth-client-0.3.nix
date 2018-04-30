{ mkDerivation, aeson, base, bytestring, doctest, Glob, hspec
, http-client, http-types, jose, QuickCheck, servant, servant-auth
, servant-auth-server, servant-client, servant-server, stdenv, text
, time, transformers, wai, warp, yaml
}:
mkDerivation {
  pname = "servant-auth-client";
  version = "0.3.0.0";
  sha256 = "870d19d3cb286c557fb811b83a2f8d2a57b21a8764f286ed83366dd2a6ca1911";
  revision = "1";
  editedCabalFile = "0y484al0hyjq496a0wdrqppc5ihr7r2qkcvkq5487655v61wg530";
  libraryHaskellDepends = [
    base bytestring servant servant-auth servant-client text
  ];
  testHaskellDepends = [
    aeson base bytestring doctest Glob hspec http-client http-types
    jose QuickCheck servant servant-auth servant-auth-server
    servant-client servant-server text time transformers wai warp yaml
  ];
  homepage = "http://github.com/plow-technologies/servant-auth#readme";
  description = "servant-client/servant-auth compatibility";
  license = stdenv.lib.licenses.bsd3;
}
