{ compiler ? "ghc864"
, nixpkgs ? (import ./nix/nixpkgs.nix { inherit compiler; })
}:

with rec {
  summoner-cliDrv = nixpkgs.haskellPackages.summoner-cli;
  summoner-tuiDrv = nixpkgs.haskellPackages.summoner-tui;
};

rec {
  inherit nixpkgs;
  summoner-cli = summoner-cliDrv;
  summoner-tui = summoner-tuiDrv;
}
