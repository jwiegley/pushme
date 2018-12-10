{ compiler    ? "ghc844"
, doBenchmark ? false
, doTracing   ? false
, doStrict    ? false
, rev         ? "3f3f6021593070330091a4a2bc785f6761bbb3c1"
, sha256      ? "1a7vvxxz8phff51vwsrdlsq5i70ig5hxvvb7lkm2lgwizgvpa6gv"
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

  overrides = self: super: {
    pipes-text = pkgs.haskell.lib.doJailbreak super.pipes-text;
  };

  source-overrides = {
  };

  modifier = drv: pkgs.haskell.lib.overrideCabal drv (attrs: {
    inherit doBenchmark;
  });

  inherit returnShellEnv;
}
