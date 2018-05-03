{ mkDerivation, base, bytestring, directory, fetchgit, hspec
, hspec-discover, network, postgresql-simple, process, stdenv
, temporary, unix
}:
mkDerivation {
  pname = "tmp-postgres";
  version = "0.2.0.0";
  src = fetchgit {
    url = "https://github.com/ajmccluskey/tmp-postgres.git";
    sha256 = "0anhjm8cxryl67sgbnw2437mxypx3sky15qhdpj8laavy3hnlcyg";
    rev = "7e90ede7edeffc03229975a90a823f9ed0af3223";
  };
  libraryHaskellDepends = [
    base bytestring directory network postgresql-simple process
    temporary unix
  ];
  testHaskellDepends = [
    base bytestring directory hspec hspec-discover postgresql-simple
    process temporary
  ];
  homepage = "https://github.com/jfischoff/tmp-postgres#readme";
  description = "Start and stop a temporary postgres for testing";
  license = stdenv.lib.licenses.bsd3;
}
