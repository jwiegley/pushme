{ compiler    ? "ghc822" # "ghc842" also works
, doProfiling ? false
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "9d0b6b9dfc92a2704e2111aa836f5bdbf8c9ba42"
, sha256      ? "096r7ylnwz4nshrfkh127dg8nhrcvgpr69l4xrdgy3kbq049r3nb"
, nixpkgs     ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
, provideDrv  ? !nixpkgs.pkgs.lib.inNixShell
}:

let inherit (nixpkgs) pkgs;

  haskellPackages' = pkgs.haskell.packages.${compiler};

  haskellPackages = pkgs.lib.fix (this: haskellPackages'.override {
    overrides = with pkgs.haskell.lib; self: super: {
      developPackage =
        { root
        , source-overrides ? {}
        , overrides ? self: super: {}
        , modifier ? drv: drv
        , provideDrv ? !pkgs.lib.inNixShell }:
        let drv =
          (this.extend
             (pkgs.lib.composeExtensions
                (self.packageSourceOverrides source-overrides)
                overrides))
          .callCabal2nix (builtins.baseNameOf root) root {};
        in if provideDrv then modifier drv else (modifier drv).env;

      pipes-group = doJailbreak super.pipes-group;
      text-format = doJailbreak super.text-format;
      enclosed-exceptions =
        if compiler == "ghc842"
        then dontCheck super.enclosed-exceptions
        else super.enclosed-exceptions;
    };
  });

in haskellPackages.developPackage {
  root = ./.;

  source-overrides =
    if compiler == "ghc802"
    then {
      lens-family-core = "1.2.1";
      lens-family = "1.2.1";
    }
    else {};

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    enableLibraryProfiling    = doProfiling;
    enableExecutableProfiling = doProfiling;

    inherit doBenchmark;
  });

  inherit provideDrv;
}
