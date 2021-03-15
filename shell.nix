{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, nixpkgsSrc ? haskellNix.sources.nixpkgs-unstable
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgsSrc nixpkgsArgs
, ghc ? "ghc8104"
, toolsGhc ? "ghc8104"
, node ? "nodejs-15_x"
, hsPkgs ? import ./default.nix { inherit pkgs ghc node; }
}: hsPkgs.shellFor {
  packages = ps: with ps; [
    inline-js
    inline-js-core
    inline-js-examples
    inline-js-tests
  ];

  withHoogle = true;

  tools = {
    haskell-language-server = "latest";
  };

  buildInputs = (builtins.map
    (t:
      (pkgs.haskell-nix.hackage-tool {
        name = t;
        compiler-nix-name = toolsGhc;
      }))
    [
      "brittany"
      "cabal"
      "cabal-fmt"
      "ghcid"
      "hindent"
      "hlint"
      "ormolu"
    ]) ++
  [
    pkgs.nixpkgs-fmt
    pkgs."${node}"
  ];

  exactDeps = true;
}
