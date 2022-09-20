{ pkgs ? import <nixpkgs> { } }:

with pkgs;

let root = builtins.toString ./.;
in mkShell {
  buildInputs = [ nodejs-16_x ];
  shellHook = ''
    export PATH=${root}/node_modules/.bin:$PATH
  '';
}
