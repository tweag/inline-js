{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs haskellNix.nixpkgsArgs
, ghc ? "ghc8105"
, toolsGhc ? "ghc8105"
}:
pkgs.callPackage
  ({ callPackage, haskell-nix, lib, runCommand }:
    runCommand "inline-js-ci"
      {
        passAsFile = [ "paths" ];
        paths = [ (callPackage ./jsbits.nix { inherit pkgs; }) ] ++ lib.concatMap
          (node: [
            (callPackage ../default.nix {
              inherit pkgs ghc node;
            }).inline-js-tests.checks.inline-js-tests
            ((callPackage ../shell.nix {
              inherit pkgs ghc toolsGhc node;
            }).overrideAttrs (_: {
              phases = [ "unpackPhase" "installPhase" ];
              src = haskell-nix.haskellLib.cleanGit {
                name = "inline-js-src";
                src = ../.;
              };
              installPhase = ''
                export HOME=$(mktemp -d)
                cabal v2-build all -j$NIX_BUILD_CORES
                cabal v2-run inline-js-tests -- -j$NIX_BUILD_CORES
                cabal-docspec
                echo :q | cabal v2-repl inline-js
                printenv > $out
              '';
            }))
          ]) [ "nodejs-16_x" "nodejs-14_x" "nodejs-12_x" "nodejs-10_x" ];
      } "mv $pathsPath > $out")
{ }
