{ pkgs ? import <nixpkgs> { }, compiler ? "ghc946" }:
let
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {
              hart = haskellPackagesNew.callPackage ./better-reads.nix { };
              openapi3 = haskellPackagesNew.callPackage ./openapi3.nix { };
            };
          };
        };
      };
    };
  };
  pkgs = import <nixpkgs> { inherit config; };
in
{
  betterReads = pkgs.haskell.packages.${compiler}.callPackage ./better-reads.nix { };
}
