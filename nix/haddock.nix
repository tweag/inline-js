{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs
, pkgsVanilla ? import haskellNix.sources.nixpkgs-unstable { }
}:
(import ../shell.nix { inherit pkgs pkgsVanilla; }).overrideAttrs (_: {
  name = "inline-js-haddock";
  phases = [ "unpackPhase" "buildPhase" "installPhase" ];
  src = import ./src.nix { };
  buildPhase = ''
    export HOME=$(mktemp -d)
    find . -name '*.cabal' -printf '%h\n' | xargs -I{} bash -ceuox pipefail 'cd {} && cabal check'
    cabal v2-sdist all
    cabal v2-haddock \
      --disable-optimization \
      -j$NIX_BUILD_CORES \
      --ghc-option=-j$(($NIX_BUILD_CORES > 4 ? 4 : $NIX_BUILD_CORES)) \
      --enable-documentation \
      --haddock-for-hackage \
      --haddock-hyperlink-source \
      --haddock-quickjump \
      all
  '';
  installPhase = ''
    mkdir -p $out/docs
    mv dist-newstyle/*-docs.tar.gz $out/docs
    mv dist-newstyle/sdist $out
  '';
  allowedReferences = [ ];
})
