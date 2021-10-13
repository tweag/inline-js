{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs haskellNix.nixpkgsArgs
, ghc ? "ghc8107"
, node ? if pkgs.stdenv.isDarwin then "nodejs-14_x" else "nodejs_latest"
, hsPkgs ? import ./nix/pkg-set.nix { inherit pkgs ghc node; }
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
    pkgs.lib.attrValues (import sources.hs-nix-tools { inherit ghc; })
    ++ [ pkgs."${node}" ];

  exactDeps = true;

  shellHook = "taskset -pc 0-1000 $$";
}
