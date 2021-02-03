{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, nixpkgsSrc ? haskellNix.sources.nixpkgs-unstable
, nixpkgsArgs ? haskellNix.nixpkgsArgs
, pkgs ? import nixpkgsSrc nixpkgsArgs
, ghc ? "ghc8103"
, node ? "nodejs-14_x"
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
