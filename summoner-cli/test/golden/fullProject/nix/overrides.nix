{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{
  fullProject = (
    with rec {
      fullProjectSource = pkgs.lib.cleanSource ../.;
      fullProjectBasic  = self.callCabal2nix "fullProject" fullProjectSource { };
    };
    overrideCabal fullProjectBasic (old: {
    })
  );
}
