{ pkgs ? import <nixpkgs> { }, compiler ? "ghc963" }:
let
  betterReads = (import ./default.nix { inherit pkgs; inherit compiler; }).betterReads;
  nix-pre-commit-hooks = import (builtins.fetchTarball "https://github.com/cachix/pre-commit-hooks.nix/tarball/master");
  pre-commit-check = nix-pre-commit-hooks.run {
    src = ./.;
    hooks = {
      ormolu.enable = true;
      shellcheck.enable = true;
      hlint.enable = true;
      cabal-fmt.enable = true;
    };
  };
in
pkgs.haskellPackages.shellFor {
  shellHook = pre-commit-check.shellHook;
  packages = hpkgs: [ betterReads ];
  nativeBuildInputs = [
    pkgs.cabal-install
    pkgs.cabal2nix
    pkgs.zlib
    pkgs.haskellPackages.haskell-language-server
    pkgs.haskellPackages.hasktags
  ];
}

