{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs
, ghc ? "ghc922"
, node ? "nodejs_latest"
, hsPkgs ? import ./nix/project.nix { inherit pkgs ghc node; }
}:
hsPkgs.shellFor {
  packages = ps:
    with ps; [
      inline-js
      inline-js-core
      inline-js-examples
      inline-js-tests
    ];

  withHoogle = true;

  nativeBuildInputs =
    pkgs.lib.attrValues (import "${sources.hs-nix-tools}/nix/tools.nix" { inherit ghc; })
    ++ [ pkgs."${node}" pkgs.util-linux ];

  exactDeps = true;

  shellHook = "taskset -pc 0-1000 $$";
}
