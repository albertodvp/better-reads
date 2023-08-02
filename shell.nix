{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc928" }:
(import ./default.nix {inherit nixpkgs compiler; }).env
