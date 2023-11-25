{ pkgs ? import <nixpkgs> { }, compiler ? "ghc946" }:
let
  betterReads = (import ./default.nix { inherit pkgs; inherit compiler; }).betterReads;
  nixPreCommitHooks = import (builtins.fetchTarball "https://github.com/cachix/pre-commit-hooks.nix/tarball/master");
  preCommitCheck = nixPreCommitHooks.run {
    src = ./.;
    hooks = {
      ormolu.enable = true;
      shellcheck.enable = true;
      cabal-fmt.enable = true;
    };
  };
in
pkgs.haskellPackages.shellFor {
  shellHook = preCommitCheck.shellHook;
  packages = hpkgs: [ betterReads ];
  nativeBuildInputs = [
    pkgs.cabal-install
    pkgs.cabal2nix
    pkgs.zlib
    pkgs.haskellPackages.haskell-language-server
    pkgs.haskellPackages.hasktags
    pkgs.haskellPackages.implicit-hie
  ];
}

