{ sources ? import ./sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs haskellNix.nixpkgsArgs
, ghc ? "ghc8107"
, node ? "nodejs_latest"
}:
pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "inline-js";
    src = ../.;
  };
  compiler-nix-name = ghc;
  modules = [
    { configureFlags = [ "-O2" ]; }
    { dontPatchELF = false; }
    { dontStrip = false; }
    { hardeningDisable = [ "all" ]; }
    {
      packages.inline-js-core.preConfigure =
        let nodeSrc = pkgs."${node}";
        in
        ''
          substituteInPlace src/Language/JavaScript/Inline/Core/NodePath.hs --replace '"node"' '"${nodeSrc}/bin/node"'
        '';
    }
    { packages.inline-js-tests.testFlags = [ "-j$NIX_BUILD_CORES" ]; }
  ];
}
