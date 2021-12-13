{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs
, pkgsVanilla ? import haskellNix.sources.nixpkgs-unstable { }
, ghc ? "ghc8107"
, node ? "nodejs_latest"
, hsPkgs ? import ./nix/project.nix { inherit pkgs pkgsVanilla ghc node; }
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
    pkgsVanilla.lib.attrValues (import sources.hs-nix-tools { inherit ghc; })
    ++ [ pkgsVanilla."${node}" pkgsVanilla.util-linux ];

  exactDeps = true;

  shellHook = "taskset -pc 0-1000 $$";
}
