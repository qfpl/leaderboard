{ mkDerivation, base, lens, selda, servant-server, stdenv, text
, warp
}:
mkDerivation {
  pname = "leaderboard";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base lens selda servant-server text ];
  executableHaskellDepends = [ base warp ];
  license = stdenv.lib.licenses.bsd3;
}
