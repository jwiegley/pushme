{ compiler    ? "ghc8107"
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "faad370edcb37162401be50d45526f52bb16a713"
, sha256      ? "1d82d4vh0layf6n925j0h2nym16jbvcvps3l5m8ln9hxn0m6gadn"
, pkgs        ? import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    inherit sha256; }) {
    config.allowUnfree = true;
    config.allowBroken = true;
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

  modifier = drv: pkgs.haskell.lib.overrideCabal
   (pkgs.haskell.lib.justStaticExecutables drv) (attrs: {
    inherit doBenchmark;
    buildTools = (attrs.buildTools or []) ++ [
      haskellPackages.cabal-install
      haskellPackages.hpack
      haskellPackages.hoogle
      haskellPackages.hasktags
    ];
  });

  inherit returnShellEnv;
}
