{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes      #-}

-- | This module contains functions for stack template creation.

module Summoner.Template
       ( createProjectTemplate
       ) where

import Named ((!))

import Summoner.Settings (Settings (..))
import Summoner.Template.Cabal (cabalFile)
import Summoner.Template.Doc (docFiles)
import Summoner.Template.GitHub (gitHubFiles)
import Summoner.Template.Haskell (haskellFiles)
import Summoner.Template.Stack (stackFiles)
import Summoner.Text (packageToModule)
import Summoner.Tree (TreeFs (..))


-- | Creating tree structure of the project.
createProjectTemplate :: Settings -> TreeFs
createProjectTemplate Settings{..} = Dir (toString settingsRepo) $
    [ cabal ]
 ++ memptyIfFalse settingsStack stack
 ++ haskell
 ++ docs
 ++ gitHub
  where
    cabal :: TreeFs
    cabal = cabalFile
        ! #repo settingsRepo
        ! #owner settingsOwner
        ! #description settingsDescription
        ! #license licenseName
        ! #fullName settingsFullName
        ! #email settingsEmail
        ! #year settingsYear
        ! #categories settingsCategories
        ! #base settingsBase
        ! #libModuleName libModuleName
        ! #github settingsGithub
        ! #isLib settingsIsLib
        ! #isExe settingsIsExe
        ! #test settingsTest
        ! #bench settingsBench
        ! #warnings settingsWarnings
        ! #extensions settingsExtensions
        ! #testedVersions settingsTestedVersions
        ! #prelude settingsPrelude

    stack :: [TreeFs]
    stack = stackFiles
        ! #prelude settingsPrelude
        ! #testedVersions settingsTestedVersions

    haskell :: [TreeFs]
    haskell = haskellFiles
        ! #libModuleName libModuleName
        ! #isLib settingsIsLib
        ! #isExe settingsIsExe
        ! #test settingsTest
        ! #bench settingsBench
        ! #stylish settingsStylish
        ! #prelude settingsPrelude

    docs :: [TreeFs]
    docs = docFiles
        ! #repo settingsRepo
        ! #owner settingsOwner
        ! #description settingsDescription
        ! #license licenseName
        ! #licenseText settingsLicenseText
        ! #contributing settingsContributing
        ! #stack settingsStack
        ! #github settingsGithub
        ! #travis settingsTravis
        ! #appVey settingsAppVey

    gitHub :: [TreeFs]
    gitHub = gitHubFiles
        ! #cabal settingsCabal
        ! #stack settingsStack
        ! #test settingsTest
        ! #github settingsGithub
        ! #travis settingsTravis
        ! #appVey settingsAppVey
        ! #testedVersions settingsTestedVersions

    licenseName, libModuleName :: Text
    licenseName   = show settingsLicenseName
    libModuleName = packageToModule settingsRepo
