{- |
Module                  : Summoner.Template.Doc
Copyright               : (c) 2017-2026 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Templates for various documentation files:

* CHANGELOG.md
* LICENSE
* README.md
-}

module Summoner.Template.Doc
       ( docFiles
       ) where

import Summoner.License (License (..), LicenseName (NONE))
import Summoner.Settings (Settings (..))
import Summoner.Tree (TreeFs (..))

import qualified Data.Text as T


docFiles :: Settings -> [TreeFs]
docFiles Settings{..} =
    [ File "README.md" readme
    , File "CHANGELOG.md" changelog ] ++
    [ File "LICENSE" (unLicense settingsLicenseText) | hasLicense ]
  where
    hasLicense :: Bool
    hasLicense = settingsLicenseName /= NONE

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
     ++ [hackageBadge]
     ++ [stackLtsBadge      | settingsStack]
     ++ [stackNightlyBadge  | settingsStack]
     ++ [licenseBadge       | hasLicense]
     ++ [""
        , settingsDescription
        ]
      where
        shieldsIo :: Text
        shieldsIo = "https://img.shields.io/"

        hackageShield, hackageLink, hackageBadge :: Text
        hackageShield = shieldsIo <> "hackage/v/" <> settingsRepo <> ".svg?logo=haskell"
        hackageLink   = "https://hackage.haskell.org/package/" <> settingsRepo
        hackageBadge  = makeBadge "Hackage" hackageShield hackageLink

        licenseShield, licenseBadge :: Text
        licenseShield = shieldsIo <> "badge/license-" <> T.replace "-" "--" licenseName <> "-blue.svg"
        licenseBadge  = makeBadge (licenseName <> " license") licenseShield "LICENSE"

        stackOrg, stackLtsShield, stackLtsLink, stackLtsBadge :: Text
        stackOrg       = "http://stackage.org/"
        stackLtsShield = stackOrg <> "package/" <> settingsRepo <> "/badge/lts"
        stackLtsLink   = stackOrg <> "lts/package/" <> settingsRepo
        stackLtsBadge  = makeBadge "Stackage Lts" stackLtsShield stackLtsLink

        stackNightlyShield, stackNightlyLink, stackNightlyBadge :: Text
        stackNightlyShield = stackOrg <> "package/" <> settingsRepo <> "/badge/nightly"
        stackNightlyLink   = stackOrg <> "nightly/package/" <> settingsRepo
        stackNightlyBadge  = makeBadge "Stackage Nightly" stackNightlyShield stackNightlyLink

        githubActionsShield, githubActionsLink, githubActionsBadge :: Text
        githubActionsShield = "https://github.com/" <> ownerRepo <> "/workflows/CI/badge.svg"
        githubActionsLink   = "https://github.com/" <> ownerRepo <> "/actions"
        githubActionsBadge  = makeBadge "GitHub CI" githubActionsShield githubActionsLink

        travisShield, travisLink, travisBadge :: Text
        travisShield = shieldsIo <> "travis/" <> ownerRepo <> ".svg?logo=travis"
        travisLink   = "https://travis-ci.com/" <> ownerRepo
        travisBadge  = makeBadge "Build status" travisShield travisLink

        appVeyorCom, appVeyorShield, appVeyorLink, appVeyorBadge :: Text
        appVeyorCom    = "https://ci.appveyor.com/"
        appVeyorShield = appVeyorCom <> "api/projects/status/github/" <> ownerRepo <> "?branch=main&svg=true"
        appVeyorLink   = appVeyorCom <> "project/" <> ownerRepo
        appVeyorBadge  = makeBadge "Windows build status" appVeyorShield appVeyorLink

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
