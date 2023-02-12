with import <nixpkgs> { };
pkgs.haskellPackages.callCabal2nix "server" ./. { }
