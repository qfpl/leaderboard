{nixpkgs, compiler}:
let
  systemNixpkgs = import <nixpkgs> { };
  defaultNixpkgs = import (systemNixpkgs.fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "bca2ee28db44bc2d2d1b5e97866ca4052d88e94f";
    sha256 = "0l9jpnkvjcj9p181ns9g4gm57n3ni4q07ynkh80r6d6qidw8cwnq";
  }) {};

  pkgs =
    if nixpkgs == null
      then systemNixpkgs
    else if nixpkgs == "default"
      then defaultNixpkgs
    else
      nixpkgs;

  haskellPackages =
    import ./modifiedHaskellPackages.nix { nixpkgs = pkgs; inherit compiler; };
  leaderboard = haskellPackages.callPackage ../leaderboard.nix {};
in
  {inherit pkgs haskellPackages leaderboard;}
