{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
}: haskellNix.pkgs-unstable
