{ nixpkgs ? import <nixpkgs> {}, ghcver ? "ghc7103" } :
let
    pkgs = nixpkgs;

    stdenv = pkgs.stdenv;

    ghcenv = pkgs.haskell.packages.${ghcver};

in
ghcenv.mkDerivation {

    pname = "evolution-hs";
    version = "0.0.1";
    src = ./.;
    isLibrary = true;
    isExecutable = false;
    license = stdenv.lib.licenses.unfree;

    buildDepends = with ghcenv ; [
      haskell-src-meta template-haskell
      filepath containers text mtl
      bytestring deepseq system-filepath text-format
      directory attoparsec mime-types
      syb parsec process optparse-applicative
      utf8-string blaze-builder alex happy
      transformers];

    doCheck = false;
}
