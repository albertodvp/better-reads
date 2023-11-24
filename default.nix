{ pkgs ? import <nixpkgs> { }, compiler ? "ghc963" }:
{
  betterReads = pkgs.haskell.packages.${compiler}.callPackage ./better-reads.nix { };
}
