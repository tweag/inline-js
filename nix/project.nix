{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs
, ghc ? "ghc922"
, node ? "nodejs_latest"
}:
pkgs.haskell-nix.cabalProject {
  src = import ./src.nix { inherit pkgs; };
  compiler-nix-name = ghc;
  modules = [
    {
      packages.inline-js-core.preConfigure =
        let nodePath = pkgs."${node}";
        in
        ''
          substituteInPlace src/Language/JavaScript/Inline/Core/NodePath.hs --replace '"node"' '"${nodePath}/bin/node"'
        '';
    }
    { packages.inline-js-tests.testFlags = [ "-j$NIX_BUILD_CORES" ]; }
  ];
}
