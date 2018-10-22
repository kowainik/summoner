{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ViewPatterns     #-}

module Summoner.Template.Doc
       ( docFiles
       ) where

import Named ((:!), arg)
import NeatInterpolation (text)

import Summoner.License (License (..))
import Summoner.Tree (TreeFs (..))

import qualified Data.Text as T


docFiles
    :: "repo"           :! Text
    -> "owner"          :! Text
    -> "description"    :! Text
    -> "license"        :! Text
    -> "licenseText"    :! License
    -> "contributing"   :! Maybe Text
    -> "stack"          :! Bool
    -> "github"         :! Bool
    -> "travis"         :! Bool
    -> "appVey"         :! Bool
    -> [TreeFs]
docFiles
    (arg #repo         -> repo)
    (arg #owner        -> owner)
    (arg #description  -> description)
    (arg #license      -> license)
    (arg #licenseText  -> licenseText)
    (arg #contributing -> contributing)
    (arg #stack        -> stack)
    (arg #github       -> github)
    (arg #travis       -> travis)
    (arg #appVey       -> appVey)
    = [ File "README.md" readme
      , File "CHANGELOG.md" changelog
      , File "LICENSE" $ unLicense licenseText
      ] ++ maybe [] (\x -> [File "CONTRIBUTING.md" x]) contributing
  where
    readme :: Text
    readme = T.intercalate "\n" $
        [ "# " <> repo
        , ""
        , hackage
        , licenseBadge
        ]
     ++ [stackLtsBadge | stack]
     ++ [stackNightlyBadge | stack]
     ++ [travisBadge | travis]
     ++ [appVeyorBadge | appVey]
     ++ [""
        , description
        ]
      where
        hackageShield :: Text =
            "https://img.shields.io/hackage/v/" <> repo <> ".svg"
        hackageLink :: Text =
            "https://hackage.haskell.org/package/" <> repo
        hackage :: Text = makeBadge "Hackage" hackageShield hackageLink

        licenseShield :: Text =
            "https://img.shields.io/badge/license-" <> T.replace "-" "--" license <> "-blue.svg"
        licenseBadge :: Text =
            makeBadge (license <> " license") licenseShield "LICENSE"

        stackShieldLts :: Text =
            "http://stackage.org/package/" <> repo <> "/badge/lts"
        stackLinkLts :: Text =
            "http://stackage.org/lts/package/" <> repo

        stackShieldNightly :: Text =
            "http://stackage.org/package/" <> repo <> "/badge/nightly"
        stackLinkNightly :: Text =
            "http://stackage.org/nightly/package/" <> repo
        stackLtsBadge :: Text =
            makeBadge "Stackage Lts" stackShieldLts stackLinkLts
        stackNightlyBadge :: Text =
            makeBadge "Stackage Nightly" stackShieldNightly stackLinkNightly

        travisShield :: Text =
            "https://secure.travis-ci.org/" <> owner <> "/" <> repo <> ".svg"
        travisLink :: Text =
            "https://travis-ci.org/" <> owner <> "/" <> repo
        travisBadge :: Text =
            makeBadge "Build status" travisShield travisLink

        appVeyorShield :: Text =
            "https://ci.appveyor.com/api/projects/status/github/" <> owner <> "/" <> repo <> "?branch=master&svg=true"
        appVeyorLink :: Text =
            "https://ci.appveyor.com/project/" <> owner <> "/" <> repo
        appVeyorBadge :: Text =
            makeBadge "Windows build status" appVeyorShield appVeyorLink

        makeBadge :: Text -> Text -> Text -> Text
        makeBadge title shield link = "[![" <> title <> "](" <> shield <> ")](" <> link <> ")"

    changelog :: Text
    changelog =
        [text|
        # Change log

        `$repo` uses [PVP Versioning][1].
        $githubLine

        0.0.0
        =====

        * Initially created.

        [1]: https://pvp.haskell.org
        $githubFootNote
        |]
      where
        githubLine :: Text = memptyIfFalse github "The change log is available [on GitHub][2]."
        githubFootNote :: Text = memptyIfFalse github $
            "[2]: https://github.com/" <> owner <> "/" <> repo <> "/releases"
