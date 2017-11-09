{ mkDerivation, aeson, base, bytestring, exceptions, fast-logger
, fetchgit, lifted-base, monad-control, stdenv, template-haskell
, text, text-show, transformers
}:
mkDerivation {
  pname = "monad-log";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/LightAndLight/monad-log.git";
    sha256 = "0czh7p7gz0gd51gggsqgxk1q5mcwypf1vr3053im90d5i0b5sc4v";
    rev = "cf394588a81347f0c4f71186e86d7c4bac610b0b";
  };
  libraryHaskellDepends = [
    aeson base bytestring exceptions fast-logger lifted-base
    monad-control template-haskell text text-show transformers
  ];
  description = "A simple and fast logging monad";
  license = stdenv.lib.licenses.mit;
}
