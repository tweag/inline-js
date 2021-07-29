{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs haskellNix.nixpkgsArgs
, ghc ? "ghc8105"
, toolsGhc ? "ghc8105"
}:
pkgs.callPackage
  ({ callPackage, haskell-nix, lib, linkFarm }:
    linkFarm "inline-js-ci" ([{
      name = "inline-js-ci-jsbits";
      path = callPackage ./jsbits.nix { inherit pkgs; };
    }] ++ lib.concatMap
      (node: [
        {
          name = "inline-js-ci-tests-${node}";
          path = (callPackage ../default.nix {
            inherit pkgs ghc node;
          }).inline-js-tests.checks.inline-js-tests;
        }
        {
          name = "inline-js-ci-shell-${node}";
          path = (callPackage ../shell.nix {
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
              touch $out
            '';
          });
        }
      ]) [ "nodejs-16_x" "nodejs-14_x" "nodejs-12_x" "nodejs-10_x" ]))
{ }
