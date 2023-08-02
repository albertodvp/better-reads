{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc928" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./better-reads.nix { }
