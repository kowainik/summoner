{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes    #-}

module Summoner.Template.Nix
       ( nixFiles
       ) where

import NeatInterpolation (text)

import Summoner.Default (defaultNixCompiler)
import Summoner.GhcVer (nixCompiler)
import Summoner.Settings (NixPkgSet (..), Settings (..), defaultNixPkgSet)
import Summoner.Tree (TreeFs (..))

import qualified Data.Text as T

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
        shellNixT =
            [text|
            (import ./default.nix {}).env
            |]

        releaseNixCompilers :: Text
        releaseNixCompilers = T.unlines
            ( map
                  (\ghcV -> let compiler = nixCompiler ghcV
                            in "  " <> compiler <> "  = import ./default.nix { compiler = \"" <> compiler <> "\";  };"
                  )
                  settingsTestedVersions
            )

        releaseNixT :: Text
        releaseNixT =
            [text|
            {
            $releaseNixCompilers
            }
            |]
 
        fetchNixpkgsNixT :: Text
        fetchNixpkgsNixT = T.unlines
            [ "{ owner  # The owner of the nixpkgs"
            , ", repo   # The repo of the nixpkgs"
            , ", rev    # The Git revision of nixpkgs to fetch"
            , ", sha256 # The SHA256 hash of the unpacked archive"
            , "}:"
            , "builtins.fetchTarball {"
            , "  url = \"https://github.com/${owner}/${repo}/archive/${rev}.tar.gz\";"
            , "  inherit sha256;"
            , "}"
            ]

        NixPkgSet { npsOwner, npsRepo, npsRev, npsSha256 } = case settingsNixPkgSet of { Just n -> n; Nothing -> defaultNixPkgSet }

        nixpkgsNixT :: Text
        nixpkgsNixT = T.unlines
            [ ("{ compiler ? \"" <> defaultNixCompiler <> "\" }:")
            , "with rec {"
            , "  fetchNixpkgs = import ./fetchNixpkgs.nix;"
            , "  nixpkgs = fetchNixpkgs {"
            , ("    owner  = \"" <> npsOwner <> "\";")
            , ("    repo   = \"" <> npsRepo <> "\";")
            , ("    rev    = \"" <> npsRev <> "\";")
            , ("    sha256 = \"" <> npsSha256 <> "\";")
            , "  };"
            , "};"
            , "import nixpkgs {"
            , "  config = {"
            , "    packageOverrides = super: let self = super.pkgs; in {"
            , "      haskellPackages = super.haskell.packages.${compiler}.override {"
            , "        overrides = import ./overrides.nix { pkgs = self; };"
            , "      };"
            , "    };"
            , "  };"
            , "  overlays = [ ];"
            , "}"
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
