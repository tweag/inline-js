{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs haskellNix.nixpkgsArgs
, ghc ? "ghc8104"
, toolsGhc ? "ghc8104"
, node ? "nodejs-16_x"
, hsPkgs ? import ./default.nix { inherit pkgs ghc node; }
}: hsPkgs.shellFor {
  packages = ps: with ps; [
    inline-js
    inline-js-core
    inline-js-examples
    inline-js-tests
  ];

  withHoogle = true;

  nativeBuildInputs = pkgs.lib.attrValues
    (pkgs.haskell-nix.tools toolsGhc {
      brittany = "latest";
      cabal = "latest";
      cabal-fmt = "latest";
      hindent = "latest";
      hlint = "latest";
      ormolu = "latest";
      stylish-haskell = "latest";
    }) ++ [
    (pkgs.haskell-nix.cabalProject {
      src = pkgs.fetchFromGitHub {
        owner = "haskell";
        repo = "haskell-language-server";
        rev = "1.1.0";
        sha256 = "0kviq3kinm3i0qm4r26rdnlkwbs1s3r1rqiqdry517rgkgnjpcp5";
        fetchSubmodules = true;
      };
      compiler-nix-name = ghc;
      configureArgs = "--disable-benchmarks --disable-tests";
    }).haskell-language-server.components.exes.haskell-language-server
    (import sources.niv { }).niv
    pkgs.nixpkgs-fmt
    pkgs."${node}"
  ];

  exactDeps = true;
}
