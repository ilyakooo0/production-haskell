{ pkgs ? import <nixpkgs> { } }:

let compilerVersion = "963";
in pkgs.mkShell {
  buildInputs = with pkgs; [
    haskell.compiler."ghc${compilerVersion}"
    (pkgs.haskell-language-server.override {
      supportedGhcVersions = [ compilerVersion ];
    })
  ];
}
