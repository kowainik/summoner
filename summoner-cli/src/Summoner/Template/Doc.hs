{-# LANGUAGE QuasiQuotes #-}

module Summoner.Template.Doc
       ( docFiles
       ) where

import Summoner.License (License (..), LicenseName (None))
import Summoner.Settings (Settings (..))
import Summoner.Tree (TreeFs (..))

import qualified Data.Text as T


docFiles :: Settings -> [TreeFs]
docFiles Settings{..} =
    [ File "README.md" readme
    , File "CHANGELOG.md" changelog ] ++
    [ File "LICENSE" (unLicense settingsLicenseText) | hasLicense ] ++
    maybeToList (File "CONTRIBUTING.md" <$> settingsContributing)
  where
    hasLicense :: Bool
    hasLicense = settingsLicenseName /= None

    licenseName :: Text
    licenseName = show settingsLicenseName

    readme :: Text
    readme = T.unlines $
        [ "# " <> settingsRepo
        , ""
        , hackage
        ]
     ++ [licenseBadge      | hasLicense]
     ++ [stackLtsBadge     | settingsStack]
     ++ [stackNightlyBadge | settingsStack]
     ++ [travisBadge       | settingsTravis]
     ++ [appVeyorBadge     | settingsAppVeyor]
     ++ [""
        , settingsDescription
        ]
      where
        hackageShield :: Text =
            "https://img.shields.io/hackage/v/" <> settingsRepo <> ".svg?logo=haskell"
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
            "https://img.shields.io/travis/" <> settingsOwner <> "/" <> settingsRepo <> ".svg?logo=travis"
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
    changelog = T.unlines $
        [ "# Changelog"
        , ""
        , "`" <> settingsRepo <> "` uses [PVP Versioning][1]."
        ] ++
        [ githubLine | settingsGitHub ] ++
        [ ""
        , "## 0.0.0.0"
        , ""
        , "* Initially created."
        , ""
        , "[1]: https://pvp.haskell.org"
        ] ++
        [ githubFootNote | settingsGitHub ]
      where
        githubLine, githubFootNote :: Text
        githubLine = "The changelog is available [on GitHub][2]."
        githubFootNote = "[2]: https://github.com/" <> settingsOwner <> "/" <> settingsRepo <> "/releases"
