{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, nixpkgsSrc ? haskellNix.sources.nixpkgs-2009
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgsSrc nixpkgsArgs
, ghc ? "ghc8102"
, node ? "nodejs-14_x"
, hsPkgs ? import ./default.nix { inherit pkgs ghc node; }
}: hsPkgs.shellFor {
  packages = ps: with ps; [
    inline-js
    inline-js-core
    inline-js-examples
    inline-js-tests
  ];

  withHoogle = true;

  buildInputs = with pkgs.haskellPackages; [
    brittany
    cabal-install
    ghcid
    hlint
    pkgs."${node}"
    (import sources.ormolu {}).ormolu
    (import sources.ghcide-nix {})."ghcide-${ghc}"
  ];

  exactDeps = true;
}
