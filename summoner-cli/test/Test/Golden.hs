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

import NeatInterpolation (text)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldReturn)

import Summoner (CustomPrelude (..), GhcVer (..), License (..), LicenseName (..), Settings (..),
                 defaultGHC, defaultNixPkgSet)
import Summoner.Template (createProjectTemplate)
import Summoner.Tree (TreeFs (..))


goldenSpec :: Spec
goldenSpec = describe "golden tests" $ do
    it "correctly creates full project" $
        checkProject fullProject
    it "correctly creates small project" $
        checkProject smallProject
  where
    checkProject pr = compareTree "test/golden" (createProjectTemplate pr) `shouldReturn` []

-- | Returns the list of the files that don't match the golden ones.
compareTree :: FilePath -> TreeFs -> IO [FilePath]
compareTree filePath (Dir name children) =
    foldlM (\l ch -> (l ++) <$> compareTree (filePath </> name) ch) [] children
compareTree filePath (File name content) = do
    let curFile = filePath </> name
    golden <- readFileText curFile
    pure $ if golden == content then [] else [curFile]

fullProject :: Settings
fullProject = Settings
    { settingsRepo           = "fullProject"
    , settingsOwner          = "kowainik"
    , settingsDescription    = "Full test project"
    , settingsFullName       = "Kowainik"
    , settingsEmail          = "xrom.xkov@gmail.com"
    , settingsYear           = "2018"
    , settingsCategories     = "Testing"
    , settingsLicenseName    = MIT
    , settingsLicenseText    = mitLicense
    , settingsGitHub         = True
    , settingsPrivate        = True
    , settingsTravis         = True
    , settingsAppVeyor       = True
    , settingsIsLib          = True
    , settingsIsExe          = True
    , settingsTest           = True
    , settingsBench          = True
    , settingsTestedVersions = [Ghc802, Ghc822, Ghc844, defaultGHC]
    , settingsBaseType       = "base-noprelude"
    , settingsPrelude        = Just $ CustomPrelude "relude" "Relude"
    , settingsExtensions     = ["ConstraintKinds", "LambdaCase", "OverloadedStrings"]
    , settingsWarnings       = ["-Wcompat", "-Widentities"]
    , settingsCabal          = True
    , settingsStack          = True
    , settingsNix            = True
    , settingsNixPkgSet      = Just defaultNixPkgSet
    , settingsStylish        = Just "This is stylish-haskell.yaml\n"
    , settingsContributing   = Just "This is contributing guide\n"
    , settingsNoUpload       = True
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

smallProject :: Settings
smallProject = Settings
    { settingsRepo           = "smallProject"
    , settingsOwner          = "kowainik"
    , settingsDescription    = "Small test project"
    , settingsFullName       = "Kowainik"
    , settingsEmail          = "xrom.xkov@gmail.com"
    , settingsYear           = "2018"
    , settingsCategories     = ""
    , settingsLicenseName    = None
    , settingsLicenseText    = ""
    , settingsGitHub         = False
    , settingsPrivate        = False
    , settingsTravis         = False
    , settingsAppVeyor       = False
    , settingsIsLib          = False
    , settingsIsExe          = True
    , settingsTest           = False
    , settingsBench          = False
    , settingsTestedVersions = [defaultGHC]
    , settingsBaseType       = "base"
    , settingsPrelude        = Nothing
    , settingsExtensions     = []
    , settingsWarnings       = []
    , settingsCabal          = True
    , settingsStack          = False
    , settingsNix            = False
    , settingsNixPkgSet      = Nothing
    , settingsStylish        = Nothing
    , settingsContributing   = Nothing
    , settingsNoUpload       = True
    }
