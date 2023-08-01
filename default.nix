{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc945" }:
nixpkgs.pkgs.haskell.packages.${compiler}.callPackage ./better-reads.nix { }
