let
  pkgs = import <nixpkgs> { };
in {
  server = pkgs.haskellPackages.callCabal2nix "server" ./. { };
}
