{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE QuasiQuotes #-}

{- | Golden tests for @summoner@.

In the @test/golden@ folder we now have two projects that are created by
@summoner@.

* @fullProject@ – the project that has all options enabled.
* @smallProject@ – the project that has limited options enabled.

The purpose of this tests is to check that with the corresponding 'Settings'
the tool creates the expected project.

-}

module Test.Golden
       ( goldenSpec
       ) where

import Data.TreeDiff (ToExpr, ansiWlEditExprCompact, ediff)
import NeatInterpolation (text)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName, (</>))
import Test.Hspec (Spec, describe, expectationFailure, it)

import Summoner (CustomPrelude (..), GhcVer (..), License (..), LicenseName (..), Settings (..),
                 defaultGHC)
import Summoner.Template (createProjectTemplate)
import Summoner.Tree (TreeFs (..))


goldenSpec :: Spec
goldenSpec = describe "golden tests" $ do
    it "correctly scaffolds the 'cabal-minimal' project" $
        checkProject "examples/cabal-minimal" cabalMinimal
    it "correctly scaffolds the 'cabal-full' project" $
        checkProject "examples/cabal-full" cabalFull
    it "correctly scaffolds the 'stack-full' project" $
        checkProject "examples/stack-full" stackFull
    it "correctly scaffolds the 'full-batteries' project" $
        checkProject "examples/full-batteries" fullBatteries
  where
    checkProject :: FilePath -> Settings -> IO ()
    checkProject path settings = do
        goldenFs  <- sortTree <$> readTreeFs path
        let testFs = sortTree $ createProjectTemplate settings
        when (goldenFs /= testFs) $ do
            putTextLn $ show $ ansiWlEditExprCompact $ ediff goldenFs testFs
            expectationFailure "Golden and scaffolded project don't match"

readTreeFs :: FilePath -> IO TreeFs
readTreeFs filePath = doesDirectoryExist filePath >>= \case
    True -> do
        dirs <- listDirectory filePath
        Dir (takeFileName filePath) <$> traverse (\dir -> readTreeFs $ filePath </> dir) dirs
    False -> do
        content <- readFileText filePath
        pure $ File (takeFileName filePath) content

sortTree :: TreeFs -> TreeFs
sortTree = \case
    file@(File _ _) -> file
    Dir path fs -> Dir path $ sort $ map sortTree fs


fullBatteries :: Settings
fullBatteries = Settings
    { settingsRepo           = "full-batteries"
    , settingsOwner          = "kowainik"
    , settingsDescription    = "Full-featured test project"
    , settingsFullName       = "Kowainik"
    , settingsEmail          = "xrom.xkov@gmail.com"
    , settingsYear           = "2020"
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
    , settingsTestedVersions = [Ghc802 .. defaultGHC]
    , settingsPrelude        = Just $ CustomPrelude "relude" "Relude"
    , settingsExtensions     = ["ConstraintKinds", "LambdaCase", "OverloadedStrings"]
    , settingsGitignore      = [".secret"]
    , settingsGhcOptions     = ["-Wcompat", "-Widentities"]
    , settingsCabal          = True
    , settingsStack          = True
    , settingsNoUpload       = True
    , settingsFiles          =
        [ File "extra.txt" "See full content of the file [here](@github)\n"
        , Dir ".github" [File "CODEOWNERS" "*  @chshersh @vrom911\n"]
        , File ".stylish-haskell.yaml" "This is stylish-haskell.yaml\n"
        , File "CONTRIBUTING.md" "This is contributing guide\n"
        ]
    }
  where
    mitLicense :: License
    mitLicense = License [text|
MIT License

Copyright (c) 2018 Kowainik

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|]

stackFull :: Settings
stackFull = Settings
    { settingsRepo           = "stack-full"
    , settingsOwner          = "kowainik"
    , settingsDescription    = "Stack-only example with all integrations"
    , settingsFullName       = "Kowainik"
    , settingsEmail          = "xrom.xkov@gmail.com"
    , settingsYear           = "2020"
    , settingsCategories     = "Testing"
    , settingsLicenseName    = BSD3
    , settingsLicenseText    = bsd3License
    , settingsGitHub         = True
    , settingsGhActions      = False
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
    }

cabalFull :: Settings
cabalFull = Settings
    { settingsRepo           = "cabal-full"
    , settingsOwner          = "kowainik"
    , settingsDescription    = "Cabal-only example with all integrations"
    , settingsFullName       = "Kowainik"
    , settingsEmail          = "xrom.xkov@gmail.com"
    , settingsYear           = "2020"
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
    }

cabalMinimal :: Settings
cabalMinimal = Settings
    { settingsRepo           = "cabal-minimal"
    , settingsOwner          = "kowainik"
    , settingsDescription    = "Minimal cabal-only test project"
    , settingsFullName       = "Kowainik"
    , settingsEmail          = "xrom.xkov@gmail.com"
    , settingsYear           = "2020"
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
    }

bsd3License :: License
bsd3License = License [text|
BSD 3-Clause License

Copyright (c) 2020, Kowainik
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
|]

-- Orphan instances

instance ToExpr TreeFs
