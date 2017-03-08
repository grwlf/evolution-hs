{ nixpkgs ? import <nixpkgs> {}, ghcver ? "ghc7103" } :
let
    pkgs = nixpkgs;

    stdenv = pkgs.stdenv;

    ghcenv = pkgs.haskell.packages.${ghcver};

    packages = ps : with ps; [
      mtl transformers text syb containers tasty tasty-quickcheck
    ];

    ghc = pkgs.haskell.packages.${ghcver}.ghcWithPackages packages;

in
ghcenv.mkDerivation {

    pname = "evolution-hs";
    version = "0.0.1";
    src = ./.;
    isLibrary = true;
    isExecutable = false;
    license = stdenv.lib.licenses.unfree;

    buildDepends = [ghc] ++ (packages ghcenv) ;

    doCheck = true;
}
