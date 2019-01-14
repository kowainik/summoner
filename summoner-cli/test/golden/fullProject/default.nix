{ compiler ? "ghc863"
, nixpkgs ? (import ./nix/nixpkgs.nix { inherit compiler; })
}:

with rec {
  drv = nixpkgs.haskellPackages.fullProject;
};

drv
