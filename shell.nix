{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc945" }:
(import ./default.nix {inherit nixpkgs compiler; }).env
