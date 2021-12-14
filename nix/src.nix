{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs
}:
pkgs.haskell-nix.haskellLib.cleanGit {
  name = "inline-js-src";
  src = ../.;
}
