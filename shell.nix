{ sources ? import ./nix/sources.nix { }
, haskellNix ? import sources.haskell-nix { }
, pkgs ? import sources.nixpkgs haskellNix.nixpkgsArgs
, ghc ? "ghc8105"
, toolsGhc ? "ghc8105"
, node ? if pkgs.stdenv.isDarwin then "nodejs-14_x" else "nodejs_latest"
, hsPkgs ? import ./default.nix { inherit pkgs ghc node; }
}:
let
  ghc_ver = pkgs.haskell-nix.compiler."${ghc}".version;
  ghc_pre_9 = !(pkgs.lib.versionAtLeast ghc_ver "9");
in
hsPkgs.shellFor {
  packages = ps:
    with ps; [
      inline-js
      inline-js-core
      inline-js-examples
      inline-js-tests
    ];

  withHoogle = true;

  tools =
    let
      args = {
        version = "latest";
        compiler-nix-name = toolsGhc;
        modules = [
          { dontPatchELF = false; }
          { dontStrip = false; }
          { hardeningDisable = [ "all" ]; }
        ];
      };
    in
    {
      brittany = args;
      cabal-fmt = args;
      floskell = args;
      ghcid = args;
      hlint = args;
      hoogle = args;
      ormolu = args;
      stylish-haskell = args;
    };

  nativeBuildInputs = [
    (pkgs.haskell-nix.cabalProject {
      src = pkgs.applyPatches {
        src = pkgs.fetchFromGitHub {
          owner = "phadej";
          repo = "cabal-extras";
          rev = "0431c270a7c5433d05d73042a689718bc9dc9c1a";
          sha256 = "sha256-xyCzfnJYZ2zi4TN/L/koz7FhYTuJ6ggVEshBcdkyV5w=";
        };
        patches = [ ./nix/cabal-extras.patch ];
      };
      compiler-nix-name = toolsGhc;
      configureArgs = "--disable-benchmarks --disable-tests";
      modules = [
        { dontPatchELF = false; }
        { dontStrip = false; }
        { hardeningDisable = [ "all" ]; }
        { reinstallableLibGhc = true; }
      ];
    }).cabal-docspec.components.exes.cabal-docspec
  ] ++ pkgs.lib.optionals ghc_pre_9 [
    (pkgs.haskell-nix.cabalProject rec {
      src = sources.haskell-language-server;
      compiler-nix-name = ghc;
      cabalProject = builtins.readFile (if ghc_pre_9 then
        "${src}/cabal.project"
      else
        "${src}/cabal-ghc901.project");
      configureArgs = "--disable-benchmarks --disable-tests";
      modules = [
        { dontPatchELF = false; }
        { dontStrip = false; }
        { hardeningDisable = [ "all" ]; }
      ];
    }).haskell-language-server.components.exes.haskell-language-server
  ] ++ [
    pkgs.haskell-nix.internal-cabal-install
    pkgs.niv
    pkgs.nixfmt
    pkgs.nixpkgs-fmt
    pkgs."${node}"
  ];

  exactDeps = true;
}
