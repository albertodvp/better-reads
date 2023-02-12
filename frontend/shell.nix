{ pkgs ? import <nixpkgs> {}}:

pkgs.mkShell {
  name="better-reads-frontend-env";
  buildInputs = [ pkgs.purescript pkgs.spago ];
}
