{
  description = "Multiple fileset synchronizer";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = import nixpkgs {
        inherit system overlays;
        inherit (haskellNix) config;
      };
      flake = pkgs.pushme.flake {
      };
      overlays = [ haskellNix.overlay
        (final: prev: {
          pushme =
            final.haskell-nix.project' {
              src = ./.;
              supportHpack = true;
              compiler-nix-name = "ghc910";
              shell = {
                tools = {
                  cabal = {};
                  haskell-language-server = {};
                  hlint = {};
                };
                buildInputs = with pkgs; [
                  pkg-config
                  haskellPackages.fourmolu
                  lefthook
                ];
              };
              modules = [{
                enableLibraryProfiling = true;
                enableProfiling = true;
              }];
            };
        })
      ];

      src = pkgs.lib.cleanSource ./.;

    in flake // {
      packages.default = flake.packages."pushme:exe:pushme";

      checks = (flake.checks or {}) // {
        build = flake.packages."pushme:exe:pushme";

        hlint = pkgs.runCommand "hlint-check" {
          nativeBuildInputs = [ pkgs.hlint ];
          inherit src;
        } ''
          cd $src
          hlint Main.hs src/
          touch $out
        '';

        fourmolu = pkgs.runCommand "fourmolu-check" {
          nativeBuildInputs = [ pkgs.haskellPackages.fourmolu ];
          inherit src;
        } ''
          cd $src
          fourmolu --mode check Main.hs src/Pushme/Options.hs
          touch $out
        '';
      };
    });
}
