{ pkgs }:

self: super:

with { inherit (pkgs.stdenv) lib; };

with pkgs.haskell.lib;

{ 
  tomland = dontCheck super.tomland_1_0_0;  

  summoner-cli = (
    with rec {
      summoner-cliSource = pkgs.lib.cleanSource ../summoner-cli/.;
      summoner-cliBasic  = self.callCabal2nix "summoner" summoner-cliSource { };
    };
    overrideCabal summoner-cliBasic (old: {
    })
  );
  
  summoner-tui = (
    with rec {
      summoner-tuiSource = pkgs.lib.cleanSource ../summoner-tui/.;
      summoner-tuiBasic  = self.callCabal2nix "summoner-tui" summoner-tuiSource { };
    };
    overrideCabal summoner-tuiBasic (old: {
    })
  );
}
