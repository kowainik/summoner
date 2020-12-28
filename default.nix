# Nix package set pinned at release-20.09 by default.
{ nixpkgs ? import (fetchGit {
  url = "https://github.com/NixOS/nixpkgs";
  ref = "release-20.09";
}) { }, compiler ? "default" }:
let

  inherit (nixpkgs) pkgs;

  haskellPackages =
    if compiler == "default"
    then pkgs.haskellPackages
    else pkgs.haskell.packages.${compiler};

  # Avoid copying files to the nix store if we are developing
  mkPkg = inShell: super: pkg: path:
    let orig = super.callCabal2nix pkg path { };
    in if inShell then orig.overrideAttrs (oldAttrs: { src = null; }) else orig;

  # Use the latest commit of microaeson and disable tests
  # The latest version cannot work with ghc > 8.6.5
  # This package is marked as broken in nixpkgs due to out-of-date dependencies
  microaeson = super:
    let
      path = builtins.fetchGit {
        url = "https://github.com/haskell-hvr/microaeson";
        rev = "8fe5b7cc84c8ce2be72be43ec20b7a18bb193f1d";
      };
      orig = super.callCabal2nix "microaeson" path { };
    in pkgs.haskell.lib.dontCheck orig;

  overrides = inShell: self: super: {
    # Libraries required
    microaeson = microaeson super;
    optparse-applicative = super.optparse-applicative_0_16_0_0;

    # Summoner itself
    summoner = mkPkg inShell super "summoner" ./summoner-cli;
    summoner-tui = mkPkg inShell super "summoner-tui" ./summoner-tui;
  };

  hPkgs = inShell: haskellPackages.override { overrides = overrides inShell; };

  # Used for developing Summoner
  shell =
    (hPkgs true).shellFor { packages = p: [ p.summoner p.summoner-tui ]; };

  # The derivations for library developers depending on nix
  drv = with (hPkgs false); { inherit summoner summoner-tui bin; };

in if pkgs.lib.inNixShell then shell else drv
