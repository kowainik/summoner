{ compiler }:
with rec {
  fetchNixpkgs = import ./fetchNixpkgs.nix;
  nixpkgs = fetchNixpkgs {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "a2187b452a8ccc4ef31a684915025141d34841ec";
    sha256 = "1hxdv6xmyka47vjlwqni8da9qyncgamxkfzg7nxh14nc72k0fdks";
  };
};

import nixpkgs {
  config = {
    packageOverrides = super: let self = super.pkgs; in {
      haskellPackages = super.haskell.packages.${compiler}.override {
        overrides = import ./overrides.nix { pkgs = self; };
      };
    };
  };
  overlays = [ ];
}
