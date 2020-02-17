{ # The git revision here corresponds to the nixpkgs-unstable channel, which at
  # the time of this writing has GHC 8.6.5 as the default compiler (matching the
  # one used by stack.yaml). Use https://howoldis.herokuapp.com/ to determine
  # the current rev.
  pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/d5bf8b23592.tar.gz") {}
  # Which GHC compiler to use.
  # To determine the list of compilers available run:
  #   nix-env -f "<nixpkgs>" -qaP -A haskell.compiler
, compiler ? "default"
}:
let
  haskellPackages =
    if compiler == "default"
      then pkgs.haskellPackages
      else pkgs.haskell.packages.${compiler};
  fetchGitHubArchive = owner: repo: rev:
    builtins.fetchTarball "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";

  # Cabal source dists should not contain symlinks targeting files outside its
  # directory. We replace such symlinks with their target here.
  unpackSymlinks = hp: pkgs.haskell.lib.overrideCabal hp (drv: {
    postUnpack = ''
      cp --remove-destination ${./README.md} $sourceRoot/README.md
      cp --remove-destination ${./CHANGELOG.md} $sourceRoot/CHANGELOG.md
      cp --remove-destination ${./LICENSE} $sourceRoot/LICENSE
    '';
  });

  # Summoner project derivation.
  projectDrv = (haskellPackages.override {
    overrides = self: super: with pkgs.haskell.lib; {
      summoner = unpackSymlinks (self.callCabal2nix "summoner" ./summoner-cli {});
      summoner-tui = unpackSymlinks (self.callCabal2nix "summoner-tui" ./summoner-tui {});
    };
  }).extend (pkgs.haskell.lib.packageSourceOverrides {
    relude = fetchGitHubArchive "kowainik" "relude"
    "4518c2a87fc2966e7922cef0c78eea1026582dc4";
    tomland = fetchGitHubArchive "kowainik" "tomland"
    "d906bd9e6d63b8063955fa7382da6fe7f10f6461";
    shellmet = fetchGitHubArchive "kowainik" "shellmet"
    "db4507020f17dc6c7253cf94ffc0bab8ab10bcd8";
    optparse-applicative = fetchGitHubArchive "pcapriotti" "optparse-applicative"
      "b861da1e6b021d6abd75ff7e9a4277939aa7a541";
  });

  # Summoner project shell.
  projectShell = projectDrv.shellFor {
    packages = p:
      [ p.summoner
        p.summoner-tui
      ];
    buildInputs =
      [ projectDrv.cabal-install
        # Dev dependencies below:
        projectDrv.ghcid
        # Runtime dependencies below;
        pkgs.curl
        pkgs.git
        pkgs.gitAndTools.hub
      ];
  };
in
if pkgs.lib.inNixShell then projectShell else projectDrv
