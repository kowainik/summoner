{- |
Module                  : Summoner.Golden
Copyright               : (c) 2021-2026 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module contains test data for golden examples.
It is used for both purposes:

  * To test new implementation against the old one
  * In the @gg@ executable to generate new golden examples boilerplate
-}

module Summoner.Golden
    ( -- * Executable scripts
      generateScripts
      -- * Default TOML config
    , generateTomlConfig
      -- * Projects
      -- ** Generator
    , generateProjects
      -- ** Project settings
    , cabalMinimal
    , cabalFull
    , stackFull
    , fullBatteries
    ) where

import Colourista (infoMessage)
import Colourista.Short (b)
import System.Directory (withCurrentDirectory)

import Summoner.CustomPrelude (CustomPrelude (..))
import Summoner.Default (defaultConfigFileContent, defaultGHC)
import Summoner.GhcVer (GhcVer (..))
import Summoner.License (License (..), LicenseName (..))
import Summoner.Settings (Settings (..), Tool (..))
import Summoner.Template (createProjectTemplate)
import Summoner.Template.Script (scriptFile)
import Summoner.Text (quote)
import Summoner.Tree (TreeFs (..), traverseTree)


generateScripts :: IO ()
generateScripts = do
    generateCabalScript
    generateStackScript

generateCabalScript :: IO ()
generateCabalScript = do
    let path = "summoner-cli/examples/cabalScript.hs"
    infoMessage $ "Writing Cabal script to: " <> toText path
    writeFileText path $ scriptFile defaultGHC Cabal

generateStackScript :: IO ()
generateStackScript = do
    let path = "summoner-cli/examples/stackScript.hs"
    infoMessage $ "Writing Stack script to: " <> toText path
    writeFileText path $ scriptFile defaultGHC Stack

generateTomlConfig :: IO ()
generateTomlConfig = do
    let path = "summoner-cli/examples/summoner-default.toml"
    infoMessage $ "Writing default TOML config to: " <> toText path
    writeFileText path defaultConfigFileContent

----------------------------------------------------------------------------
-- Entire projects
----------------------------------------------------------------------------

generateProjects :: IO ()
generateProjects = withCurrentDirectory "summoner-cli/examples" $ do
    mkProject cabalMinimal
    mkProject cabalFull
    mkProject stackFull
    mkProject fullBatteries

mkProject :: Settings -> IO ()
mkProject settings = do
    infoMessage $ "Creating project: " <> b (settingsRepo settings)
    traverseTree $ createProjectTemplate settings

cabalMinimal :: Settings
cabalMinimal = Settings
    { settingsRepo           = "cabal-minimal"
    , settingsOwner          = "kowainik"
    , settingsDescription    = "Minimal cabal-only test project"
    , settingsFullName       = "Kowainik"
    , settingsEmail          = "xrom.xkov@gmail.com"
    , settingsYear           = "2022"
    , settingsCategories     = ""
    , settingsLicenseName    = NONE
    , settingsLicenseText    = ""
    , settingsGitHub         = False
    , settingsGhActions      = False
    , settingsPrivate        = False
    , settingsTravis         = False
    , settingsAppVeyor       = False
    , settingsIsLib          = False
    , settingsIsExe          = True
    , settingsTest           = False
    , settingsBench          = False
    , settingsTestedVersions = [defaultGHC]
    , settingsPrelude        = Nothing
    , settingsExtensions     = []
    , settingsGhcOptions     = []
    , settingsGitignore      = []
    , settingsCabal          = True
    , settingsStack          = False
    , settingsNoUpload       = True
    , settingsFiles          = mempty
    , settingsBranchName     = "main"
    }

cabalFull :: Settings
cabalFull = Settings
    { settingsRepo           = "cabal-full"
    , settingsOwner          = "kowainik"
    , settingsDescription    = "Cabal-only example with all integrations"
    , settingsFullName       = "Kowainik"
    , settingsEmail          = "xrom.xkov@gmail.com"
    , settingsYear           = "2022"
    , settingsCategories     = "Testing"
    , settingsLicenseName    = BSD3
    , settingsLicenseText    = bsd3License
    , settingsGitHub         = True
    , settingsGhActions      = True
    , settingsPrivate        = False
    , settingsTravis         = True
    , settingsAppVeyor       = True
    , settingsIsLib          = True
    , settingsIsExe          = True
    , settingsTest           = True
    , settingsBench          = True
    , settingsTestedVersions = [Ghc844 .. defaultGHC]
    , settingsPrelude        = Just $ CustomPrelude "relude" "Relude"
    , settingsExtensions     = ["DeriveGeneric", "TypeApplications"]
    , settingsGitignore      = []
    , settingsGhcOptions     = []
    , settingsCabal          = True
    , settingsStack          = False
    , settingsNoUpload       = False
    , settingsFiles          = []
    , settingsBranchName     = "main"
    }

stackFull :: Settings
stackFull = Settings
    { settingsRepo           = "stack-full"
    , settingsOwner          = "kowainik"
    , settingsDescription    = "Stack-only example with all integrations"
    , settingsFullName       = "Kowainik"
    , settingsEmail          = "xrom.xkov@gmail.com"
    , settingsYear           = "2022"
    , settingsCategories     = "Testing"
    , settingsLicenseName    = BSD3
    , settingsLicenseText    = bsd3License
    , settingsGitHub         = True
    , settingsGhActions      = True
    , settingsPrivate        = False
    , settingsTravis         = True
    , settingsAppVeyor       = True
    , settingsIsLib          = True
    , settingsIsExe          = True
    , settingsTest           = True
    , settingsBench          = True
    , settingsTestedVersions = [Ghc844 .. defaultGHC]
    , settingsPrelude        = Just $ CustomPrelude "relude" "Relude"
    , settingsExtensions     = ["DeriveGeneric", "TypeApplications"]
    , settingsGitignore      = []
    , settingsGhcOptions     = []
    , settingsCabal          = False
    , settingsStack          = True
    , settingsNoUpload       = False
    , settingsFiles          = []
    , settingsBranchName     = "master"
    }

fullBatteries :: Settings
fullBatteries = Settings
    { settingsRepo           = "full-batteries"
    , settingsOwner          = "kowainik"
    , settingsDescription    = "Full-featured test project"
    , settingsFullName       = "Kowainik"
    , settingsEmail          = "xrom.xkov@gmail.com"
    , settingsYear           = "2022"
    , settingsCategories     = "Testing"
    , settingsLicenseName    = MIT
    , settingsLicenseText    = mitLicense
    , settingsGitHub         = True
    , settingsGhActions      = True
    , settingsPrivate        = True
    , settingsTravis         = True
    , settingsAppVeyor       = True
    , settingsIsLib          = True
    , settingsIsExe          = True
    , settingsTest           = True
    , settingsBench          = True
    , settingsTestedVersions = [minBound .. defaultGHC]
    , settingsPrelude        = Just $ CustomPrelude "relude" "Relude"
    , settingsExtensions     = ["ConstraintKinds", "LambdaCase", "OverloadedStrings"]
    , settingsGitignore      = [".secret"]
    , settingsGhcOptions     = ["-Wcompat", "-Widentities"]
    , settingsCabal          = True
    , settingsStack          = True
    , settingsNoUpload       = True
    , settingsFiles          =
        [ File "extra.txt" "See full content of the file [here](@github)\n"
        , Dir ".github" [File "CODEOWNERS" "*  @vrom911\n"]
        , File ".stylish-haskell.yaml" "This is stylish-haskell.yaml\n"
        , File "CONTRIBUTING.md" "This is contributing guide\n"
        ]
    , settingsBranchName     = "main"
    }
  where
    mitLicense :: License
    mitLicense = License  $ unlines
        [ "MIT License"
        , ""
        , "Copyright (c) 2022 Kowainik"
        , ""
        , "Permission is hereby granted, free of charge, to any person obtaining a copy"
        , "of this software and associated documentation files (the " <> quote "Software" <> "), to deal"
        , "in the Software without restriction, including without limitation the rights"
        , "to use, copy, modify, merge, publish, distribute, sublicense, and/or sell"
        , "copies of the Software, and to permit persons to whom the Software is"
        , "furnished to do so, subject to the following conditions:"
        , ""
        , "The above copyright notice and this permission notice shall be included in all"
        , "copies or substantial portions of the Software."
        , ""
        , "THE SOFTWARE IS PROVIDED " <> quote "AS IS" <> ", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR"
        , "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,"
        , "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE"
        , "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER"
        , "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,"
        , "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE"
        , "SOFTWARE."
        ]

bsd3License :: License
bsd3License = License $ unlines
    [ "BSD 3-Clause License"
    , ""
    , "Copyright (c) 2022, Kowainik"
    , "All rights reserved."
    , ""
    , "Redistribution and use in source and binary forms, with or without"
    , "modification, are permitted provided that the following conditions are met:"
    , ""
    , "1. Redistributions of source code must retain the above copyright notice, this"
    , "   list of conditions and the following disclaimer."
    , ""
    , "2. Redistributions in binary form must reproduce the above copyright notice,"
    , "   this list of conditions and the following disclaimer in the documentation"
    , "   and/or other materials provided with the distribution."
    , ""
    , "3. Neither the name of the copyright holder nor the names of its"
    , "   contributors may be used to endorse or promote products derived from"
    , "   this software without specific prior written permission."
    , ""
    , "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS " <> quote "AS IS"
    , "AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE"
    , "IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE"
    , "DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE"
    , "FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL"
    , "DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR"
    , "SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER"
    , "CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,"
    , "OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE"
    , "OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."
    ]
