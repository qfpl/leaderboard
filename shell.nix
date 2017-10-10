{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:
(import ./default.nix { inherit nixpkgs; inherit compiler; }).env
