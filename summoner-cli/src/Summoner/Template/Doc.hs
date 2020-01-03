{- |
Copyright: (c) 2017-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Templates for various documentation files:

* CHANGELOG.md
* CONTRIBUTING.md
* LICENSE
* README.md
-}

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
    readme = unlines $
        [ "# " <> settingsRepo
        , ""
        ]
     ++ [githubActionsBadge | settingsGhActions]
     ++ [travisBadge        | settingsTravis]
     ++ [appVeyorBadge      | settingsAppVeyor]
     ++ [hackage]
     ++ [stackLtsBadge      | settingsStack]
     ++ [stackNightlyBadge  | settingsStack]
     ++ [licenseBadge       | hasLicense]
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

        githubActionsShield, githubActionsLink, githubActionsBadge :: Text
        githubActionsShield = "https://github.com/" <> ownerRepo <> "/workflows/CI/badge.svg"
        githubActionsLink   = "https://github.com/" <> ownerRepo <> "/actions"
        githubActionsBadge  = makeBadge "GitHub CI" githubActionsShield githubActionsLink

        travisShield :: Text =
            "https://img.shields.io/travis/" <> ownerRepo <> ".svg?logo=travis"
        travisLink :: Text =
            "https://travis-ci.org/" <> ownerRepo
        travisBadge :: Text =
            makeBadge "Build status" travisShield travisLink

        appVeyorShield :: Text =
            "https://ci.appveyor.com/api/projects/status/github/" <> ownerRepo <> "?branch=master&svg=true"
        appVeyorLink :: Text =
            "https://ci.appveyor.com/project/" <> ownerRepo
        appVeyorBadge :: Text =
            makeBadge "Windows build status" appVeyorShield appVeyorLink

        makeBadge :: Text -> Text -> Text -> Text
        makeBadge title shield link = "[![" <> title <> "](" <> shield <> ")](" <> link <> ")"

        ownerRepo :: Text
        ownerRepo = settingsOwner <> "/" <> settingsRepo

    changelog :: Text
    changelog = unlines $
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
