{-# LANGUAGE QuasiQuotes #-}

module Summoner.Template.Doc
       ( docFiles
       ) where

import NeatInterpolation (text)

import Summoner.License (License (..), LicenseName (None))
import Summoner.Settings (Settings (..))
import Summoner.Tree (TreeFs (..))

import qualified Data.Text as T


docFiles :: Settings -> [TreeFs]
docFiles Settings{..} =
    [ File "README.md" readme
    , File "CHANGELOG.md" changelog
    ] ++ [File "LICENSE" (unLicense settingsLicenseText) | isNotNoneLicense]
      ++ maybeToList (File "CONTRIBUTING.md" <$> settingsContributing)
  where
    isNotNoneLicense :: Bool
    isNotNoneLicense = settingsLicenseName /= None

    licenseName :: Text
    licenseName = show settingsLicenseName

    readme :: Text
    readme = T.intercalate "\n" $
        [ "# " <> settingsRepo
        , ""
        , hackage
        ]
     ++ [licenseBadge      | isNotNoneLicense]
     ++ [stackLtsBadge     | settingsStack]
     ++ [stackNightlyBadge | settingsStack]
     ++ [travisBadge       | settingsTravis]
     ++ [appVeyorBadge     | settingsAppVeyor]
     ++ [""
        , settingsDescription
        ]
      where
        hackageShield :: Text =
            "https://img.shields.io/hackage/v/" <> settingsRepo <> ".svg"
        hackageLink :: Text =
            "https://hackage.haskell.org/package/" <> settingsRepo
        hackage :: Text = makeBadge "Hackage" hackageShield hackageLink

        licenseShield :: Text =
            "https://img.shields.io/badge/license-" <> T.replace "-" "--" licenseName <> "-blue.svg"
        licenseBadge :: Text =
            makeBadge (licenseName <> " license") licenseShield "LICENSE"

        stackShieldLts :: Text =
            "http://stackage.org/package/" <> settingsRepo <> "/badge/lts"
        stackLinkLts :: Text =
            "http://stackage.org/lts/package/" <> settingsRepo

        stackShieldNightly :: Text =
            "http://stackage.org/package/" <> settingsRepo <> "/badge/nightly"
        stackLinkNightly :: Text =
            "http://stackage.org/nightly/package/" <> settingsRepo
        stackLtsBadge :: Text =
            makeBadge "Stackage Lts" stackShieldLts stackLinkLts
        stackNightlyBadge :: Text =
            makeBadge "Stackage Nightly" stackShieldNightly stackLinkNightly

        travisShield :: Text =
            "https://secure.travis-ci.org/" <> settingsOwner <> "/" <> settingsRepo <> ".svg"
        travisLink :: Text =
            "https://travis-ci.org/" <> settingsOwner <> "/" <> settingsRepo
        travisBadge :: Text =
            makeBadge "Build status" travisShield travisLink

        appVeyorShield :: Text =
            "https://ci.appveyor.com/api/projects/status/github/" <> settingsOwner <> "/" <> settingsRepo <> "?branch=master&svg=true"
        appVeyorLink :: Text =
            "https://ci.appveyor.com/project/" <> settingsOwner <> "/" <> settingsRepo
        appVeyorBadge :: Text =
            makeBadge "Windows build status" appVeyorShield appVeyorLink

        makeBadge :: Text -> Text -> Text -> Text
        makeBadge title shield link = "[![" <> title <> "](" <> shield <> ")](" <> link <> ")"

    changelog :: Text
    changelog = T.stripEnd
        [text|
        # Changelog

        `$settingsRepo` uses [PVP Versioning][1].
        $githubLine

        0.0.0
        =====

        * Initially created.

        [1]: https://pvp.haskell.org
        $githubFootNote
        |]
      where
        githubLine :: Text = memptyIfFalse settingsGitHub "The changelog is available [on GitHub][2]."
        githubFootNote :: Text = memptyIfFalse settingsGitHub $
            "[2]: https://github.com/" <> settingsOwner <> "/" <> settingsRepo <> "/releases"
