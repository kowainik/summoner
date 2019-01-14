{ compiler ? "ghc863" }:
with rec {
  fetchNixpkgs = import ./fetchNixpkgs.nix;
  nixpkgs = fetchNixpkgs {
    owner  = "NixOS";
    repo   = "nixpkgs";
    rev    = "cecec1f74468766825c2ad32d8388c2ded36225f";
    sha256 = "1sq538wy0shbakah27b6n4bl5amzwkzjsds77vdd8rsq0d1nys4w";
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
