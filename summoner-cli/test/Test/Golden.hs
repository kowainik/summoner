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
    it "correctly creates full project" $
        checkProject "test/golden/fullProject" fullProject
    it "correctly creates small project" $
        checkProject "test/golden/smallProject" smallProject
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
    , settingsGitignore      = [".secret"]
    , settingsGhcOptions     = ["-Wcompat", "-Widentities"]
    , settingsCabal          = True
    , settingsStack          = True
    , settingsStylish        = Just "This is stylish-haskell.yaml\n"
    , settingsContributing   = Just "This is contributing guide\n"
    , settingsNoUpload       = True
    , settingsFiles          = mempty
--    , settingsFiles          = Map.fromList [("extra.txt", Link "@github")]
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
    , settingsGhcOptions     = []
    , settingsGitignore      = []
    , settingsCabal          = True
    , settingsStack          = False
    , settingsStylish        = Nothing
    , settingsContributing   = Nothing
    , settingsNoUpload       = True
    , settingsFiles          = mempty
    }

-- Orphan instances

instance ToExpr TreeFs
