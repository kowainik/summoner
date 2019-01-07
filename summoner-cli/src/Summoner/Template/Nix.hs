{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}

module Summoner.Template.Nix
       ( nixFiles
       ) where

import NeatInterpolation (text)

import Summoner.Default (defaultGHC)
import Summoner.GhcVer (GhcVer (..))
import Summoner.Settings (Settings (..), NixPkgSet (..), defaultNixPkgSet)
import Summoner.Tree (TreeFs (..))

import qualified Data.Text as T

nixCompiler :: GhcVer -> Text
nixCompiler = \case
  Ghc7103 -> "ghc7103"
  Ghc801  -> "ghc802"
  Ghc802  -> "ghc802"
  Ghc822  -> "ghc822"
  Ghc843  -> "ghc844" 
  Ghc844  -> "ghc844"
  Ghc863  -> "ghc863"

defaultNixCompiler :: Text
defaultNixCompiler = nixCompiler defaultGHC

nixFiles :: Settings -> [TreeFs]
nixFiles Settings{..} =
  [ shellNix
  , releaseNix
  , defaultNix
  , nixDir
  ]
  where
    defaultNix = File "default.nix" defaultNixT
    shellNix = File "shell.nix" shellNixT
    releaseNix = File "release.nix" releaseNixT

    nixDir = Dir "nix" [fetchNixpkgsNix, nixpkgsNix, overridesNix]
    fetchNixpkgsNix = File "fetchNixpkgs.nix" fetchNixpkgsNixT
    nixpkgsNix = File "nixpkgs.nix" nixpkgsNixT
    overridesNix = File "overrides.nix" overridesNixT

    defaultNixT :: Text
    defaultNixT =
      [text|
      { compiler ? "${defaultNixCompiler}"
      , nixpkgs ? (import ./nix/nixpkgs.nix { inherit compiler; })
      }:

      with rec {
        drv = nixpkgs.haskellPackages.${settingsRepo};
      };

      drv
      |]

    shellNixT :: Text
    shellNixT = "(import ./default.nix {}).env" <> endLine

    releaseNixT :: Text
    releaseNixT = T.concat
      [ "{"
      , endLine
      , T.intercalate endLine
          ( map 
            (\ghcV -> let compiler = nixCompiler ghcV
                      in compiler <> "  = import ./default.nix { compiler = \"" <> compiler <> "\";  };"
            )
            settingsTestedVersions
          )
      , endLine
      , "}"
      , endLine
      ]

    fetchNixpkgsNixT :: Text
    fetchNixpkgsNixT = T.concat
      [ "{ owner  # The owner of the nixpkgs", endLine
      , ", repo   # The repo of the nixpkgs", endLine 
      , ", rev    # The Git revision of nixpkgs to fetch", endLine
      , ", sha256 # The SHA256 hash of the unpacked archive", endLine
      , "}:", endLine, endLine
      , "builtins.fetchTarball {", endLine
      , "  url = \"https://github.com/${owner}/${repo}/archive/${rev}.tar.gz\";", endLine
      , "  inherit sha256;", endLine
      , "}"
      , endLine
      ]

    NixPkgSet { npsOwner, npsRepo, npsRev, npsSha } = case settingsNixPkgSet of { Just n -> n; Nothing -> defaultNixPkgSet }

    nixpkgsNixT :: Text
    nixpkgsNixT = T.concat
      [ "{ compiler ? \"", defaultNixCompiler, "\" }:", endLine, endLine
      , "with rec {", endLine
      , "  fetchNixpkgs = import ./fetchNixpkgs.nix;", endLine
      , "  nixpkgs = fetchNixpkgs {", endLine
      , "    owner  = \"", npsOwner, "\";", endLine
      , "    repo   = \"", npsRepo, "\";", endLine
      , "    rev    = \"", npsRev, "\";", endLine
      , "    sha256 = \"", npsSha, "\";", endLine
      , "  };", endLine
      , "};", endLine, endLine
      , "import nixpkgs {", endLine
      , "  config = {", endLine
      , "    packageOverrides = super: let self = super.pkgs; in {", endLine
      , "      haskellPackages = super.haskell.packages.${compiler}.override {", endLine
      , "        overrides = import ./overrides.nix { pkgs = self; };", endLine
      , "      };", endLine
      , "    };", endLine
      , "  };", endLine
      , "  overlays = [ ];", endLine
      , "}"
      , endLine
      ]

    overridesNixT :: Text
    overridesNixT =
      [text|
      { pkgs }:

      self: super:

      with { inherit (pkgs.stdenv) lib; };

      with pkgs.haskell.lib;

      {
        $settingsRepo = (
          with rec {
            ${settingsRepo}Source = pkgs.lib.cleanSource ../.;
            ${settingsRepo}Basic  = self.callCabal2nix "${settingsRepo}" ${settingsRepo}Source { };  
          };
          overrideCabal ${settingsRepo}Basic (old: {
          })
        );
      }
      |]