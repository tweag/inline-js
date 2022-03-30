{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs
, ghcs ? [ "ghc865" "ghc884" "ghc8107" "ghc902" "ghc922" ]
, nodes ? [
    "nodejs-17_x"
    "nodejs-16_x"
    "nodejs-14_x"
    "nodejs-12_x"
  ]
}:
pkgs.callPackage
  ({ lib, runCommand, stdenvNoCC }:
    runCommand "inline-js-ci"
      {
        paths = [
          (import ./jsbits.nix { inherit pkgs; })
          (import ./haddock.nix { inherit pkgs; })
        ] ++ lib.concatMap
          (ghc:
            lib.concatMap
              (node:
                [
                  ((import ../shell.nix {
                    inherit pkgs ghc node;
                  }).overrideAttrs (_: {
                    name = "inline-js-ci-${ghc}-${node}";
                    phases = [ "unpackPhase" "buildPhase" ];
                    src = import ./src.nix { inherit pkgs; };
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
              nodes)
          ghcs;
      } "export > $out")
{ }
