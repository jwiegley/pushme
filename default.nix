{ compiler    ? "ghc884"
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "502845c3e31ef3de0e424f3fcb09217df2ce6df6"
, sha256      ? "0fcqpsy6y7dgn0y0wgpa56gsg0b0p8avlpjrd79fp4mp9bl18nda"
, pkgs        ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = false;
  }
, returnShellEnv ? pkgs.lib.inNixShell
, mkDerivation ? null
}:

let haskellPackages = pkgs.haskell.packages.${compiler};

in haskellPackages.developPackage {
  root = ./.;

  overrides = self: super: with pkgs.haskell.lib; {
    pipes-text      = markUnbroken (doJailbreak super.pipes-text);
    time-compat     = doJailbreak super.time-compat;
    time-recurrence = unmarkBroken (doJailbreak super.time-recurrence);
  };

  source-overrides = {
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    inherit doBenchmark;
    buildTools = (attrs.buildTools or []) ++ [
      haskellPackages.cabal-install
      haskellPackages.hpack
      haskellPackages.hoogle
      haskellPackages.hasktags
      haskellPackages.ghcid
      haskellPackages.ormolu
    ];
  });

  inherit returnShellEnv;
}
