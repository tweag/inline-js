{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import haskellNix.sources.nixpkgs-unstable haskellNix.nixpkgsArgs
, pkgsVanilla ? import haskellNix.sources.nixpkgs-unstable { }
, ghc ? "ghc8107"
, node ? "nodejs_latest"
}:
pkgs.haskell-nix.cabalProject {
  src = import ./src.nix { inherit pkgs; };
  compiler-nix-name = ghc;
  modules = [
    {
      packages.inline-js-core.preConfigure =
        let nodePath = pkgsVanilla."${node}";
        in
        ''
          substituteInPlace src/Language/JavaScript/Inline/Core/NodePath.hs --replace '"node"' '"${nodePath}/bin/node"'
        '';
    }
    { packages.inline-js-tests.testFlags = [ "-j$NIX_BUILD_CORES" ]; }
  ];
}
