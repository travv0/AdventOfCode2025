{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    haskell.compiler.ghc96
    cabal-install
    haskell.packages.ghc96.haskell-language-server
    hlint
  ];

  shellHook = ''
    echo "Advent of Code 2025 - Haskell Development Environment"
    echo "GHC version: $(ghc --version)"
    echo "Cabal version: $(cabal --version | head -1)"
  '';
}
