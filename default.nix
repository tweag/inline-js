{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? haskellNix.pkgs-unstable
, ghc ? "ghc8104"
, node ? "nodejs-15_x"
}: pkgs.haskell-nix.cabalProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "inline-js";
    src = ./.;
  };
  compiler-nix-name = ghc;
  modules = let nodeSrc = pkgs."${node}"; in
    [{
      packages.inline-js-core.configureFlags = [
        ''--ghc-option=-DINLINE_JS_NODE=\"${nodeSrc}/bin/node\"''
      ];
    }];
}
