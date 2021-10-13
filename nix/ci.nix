{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs haskellNix.nixpkgsArgs
, ghcs ? [ "ghc865" "ghc884" "ghc8107" ]
}:
pkgs.callPackage
  ({ callPackage, haskell-nix, lib, runCommand, stdenvNoCC }:
    let
      src = haskell-nix.haskellLib.cleanGit {
        name = "inline-js-src";
        src = ../.;
      };
    in
    runCommand "inline-js-ci"
      {
        paths = [ (callPackage ./jsbits.nix { inherit pkgs; }) ] ++ lib.concatMap
          (ghc:
            lib.concatMap
              (node: [
                (callPackage ./pkg-set.nix {
                  inherit pkgs ghc node;
                }).inline-js-tests.checks.inline-js-tests
                ((callPackage ../shell.nix { inherit pkgs ghc node; }).overrideAttrs
                  (_: {
                    name = "inline-js-ci-${ghc}-${node}";
                    phases = [ "unpackPhase" "buildPhase" ];
                    inherit src;
                    buildPhase = ''
                      export HOME=$(mktemp -d)
                      cabal v2-build all -j$NIX_BUILD_CORES --ghc-option=-j$(($NIX_BUILD_CORES > 4 ? 4 : $NIX_BUILD_CORES))
                      cabal v2-run inline-js-tests -- -j$NIX_BUILD_CORES
                      cabal-docspec
                      echo :q | cabal v2-repl inline-js
                      export > $out
                    '';
                  }))
              ])
              (lib.optionals (!stdenvNoCC.isDarwin) [ "nodejs-16_x" ]
                ++ [ "nodejs-14_x" "nodejs-12_x" "nodejs-10_x" ]))
          ghcs;
      } "export > $out")
{ }
