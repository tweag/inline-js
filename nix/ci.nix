{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs
, pkgsVanilla ? import haskellNix.sources.nixpkgs-unstable { }
, ghcs ? [ "ghc865" "ghc884" "ghc8107" "ghc901" ]
}:
pkgsVanilla.callPackage
  ({ lib, runCommand, stdenvNoCC }:
    let
      src = pkgs.haskell-nix.haskellLib.cleanGit {
        name = "inline-js-src";
        src = ../.;
      };
    in
    runCommand "inline-js-ci"
      {
        paths = [ (import ./jsbits.nix { inherit pkgs pkgsVanilla; }) ]
          ++ lib.concatMap
          (ghc:
            lib.concatMap
              (node:
                [
                  ((import ../shell.nix {
                    inherit pkgs pkgsVanilla ghc node;
                  }).overrideAttrs (_: {
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
                ]) [
              "nodejs-17_x"
              "nodejs-16_x"
              "nodejs-14_x"
              "nodejs-12_x"
              "nodejs-10_x"
            ])
          ghcs;
      } "export > $out")
{ }
