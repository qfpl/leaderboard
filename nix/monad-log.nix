{ mkDerivation, aeson, base, bytestring, exceptions, fast-logger
, fetchgit, lifted-base, monad-control, stdenv, template-haskell
, text, text-show, transformers
}:
mkDerivation {
  pname = "monad-log";
  version = "0.1.2.0";
  src = fetchgit {
    url = "https://github.com/ajmccluskey/monad-log.git";
    sha256 = "1zc7vdkhjqvgpjk5nh3qybrzz0v50a11p6g8sksji6vlvkqbx6zd";
    rev = "c740bf06e85031fcb0d89e393c86f449d8204cfb";
  };
  libraryHaskellDepends = [
    aeson base bytestring exceptions fast-logger lifted-base
    monad-control template-haskell text text-show transformers
  ];
  description = "A simple and fast logging monad";
  license = stdenv.lib.licenses.mit;
}
