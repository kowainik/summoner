{-# LANGUAGE QuasiQuotes #-}

-- | This module introduces functional for project creation.

module Summoner.Project
       ( generateProject
       ) where

import Control.Monad (when)
import Data.Aeson (decodeStrict)
import Data.ByteString.Char8 (pack)
import Data.List (nub)
import Data.Semigroup ((<>))
import Data.Text (Text)
import NeatInterpolation (text)
import System.Info (os)
import System.Process (readProcess)

import Summoner.Ansi (successMessage, warningMessage)
import Summoner.Config (Config, ConfigP (..))
import Summoner.Default (currentYear, defaultGHC, defaultLicense)
import Summoner.License (License (..), customizeLicense, githubLicenseQueryNames, licenseNames)
import Summoner.Process (deleteFile)
import Summoner.ProjectData (Decision (..), ProjectData (..), parseGhcVer, showGhcVer,
                             supportedGhcVers)
import Summoner.Question (checkUniqueName, choose, query, queryDef, queryManyRepeatOnFail)
import Summoner.Template (createStackTemplate)

import qualified Data.Text as T
import qualified Data.Text.IO as T

decisionToBool :: Decision -> Text -> IO Bool
decisionToBool decision target = case decision of
    Yes -> trueMessage  target
    Nop -> falseMessage target
    Idk -> do
        ch <- choose ("Add " <> target <> "?") ["y", "n"]
        case ch of
            "y" -> trueMessage  target
            "n" -> falseMessage target
            _   -> error "Impossible happened"

trueMessage, falseMessage :: Text -> IO Bool
trueMessage  target = True  <$ successMessage (T.toTitle target <> " will be added to the project")
falseMessage target = False <$ warningMessage (T.toTitle target <> " won't be added to the project")


-- | Generate the project.
generateProject :: Text -> Config -> IO ()
generateProject projectName Config{..} = do
    repo        <- checkUniqueName projectName
    owner       <- queryDef "Repository owner: " cOwner
    description <- query "Short project description: "
    nm          <- queryDef "Author: " cFullName
    email       <- queryDef "Maintainer e-mail: " cEmail
    T.putStr categoryText
    category <- query "Category: "
    license  <- choose "License: " $ map unLicense $ nub (defaultLicense : licenseNames)

    -- License creation
    let licenseGithub = snd
                      $ head
                      $ dropWhile ((/= license) . unLicense . fst) githubLicenseQueryNames
    let licenseLink = "https://api.github.com/licenses/" <> licenseGithub
    licenseJson <-
      readProcess "curl"
                  [ T.unpack licenseLink
                  , "-H"
                  , "Accept: application/vnd.github.drax-preview+json"
                  ]
                  ""
    year <- currentYear
    let licenseText = case (decodeStrict $ pack licenseJson) :: Maybe License of
            Just t  -> customizeLicense license (unLicense t) nm year
            Nothing -> error "Broken predefined license list"

    -- Library/Executable/Tests/Benchmarks flags
    github <- decisionToBool cGitHub "github integration"
    travis <- ifGithub github "Travis CI integration" cTravis
    appVey <- ifGithub github "AppVeyor CI integration" cAppVey
    privat <- ifGithub github "Private repository" cPrivate
    script <- decisionToBool cScript "build script"
    isLib  <- decisionToBool cLib "library target"
    isExe  <- let target = "executable target" in
              if isLib
              then decisionToBool cExe target
              else trueMessage target
    test   <- decisionToBool cTest "tests"
    bench  <- decisionToBool cBench "benchmarks"

    T.putStrLn $ "Supported by 'summoner' GHCs: " <> T.intercalate " " (map showGhcVer supportedGhcVers)
    T.putStrLn $ "The project will be created with the latest resolver for default GHC-" <> showGhcVer defaultGHC
    testedVersions <- queryManyRepeatOnFail
        parseGhcVer
        "Additionally you can specify versions of GHC to test with (space-separated): "

    -- Create project data from all variables in scope
    let projectData = ProjectData{..}

    -- create stack project
    doStackCommands projectData
    -- make b executable
    when script doScriptCommand
    -- create github repository and commit
    when github $ doGithubCommands projectData privat

 where
    ifGithub :: Bool -> Text -> Decision -> IO Bool
    ifGithub github target decision = if github
        then decisionToBool decision target
        else falseMessage target

    doStackCommands :: ProjectData -> IO ()
    doStackCommands projectData@ProjectData{..} = do
        -- create haskell template
        T.writeFile "temp.hsfiles" $ createStackTemplate projectData
        -- create new project with stack
        "stack" ["new", repo, "temp.hsfiles"]
        -- do not need template file anymore
        deleteFile "temp.hsfiles"
        "cd" [repo]

    doScriptCommand :: IO ()
    doScriptCommand = when (os /= "mingw32") ("chmod" ["+x", "b"])

    doGithubCommands :: ProjectData -> Bool -> IO ()
    doGithubCommands ProjectData{..} private = do
        -- Create the repository on Github.
        "git" ["init"]
        "hub" $ ["create", "-d", description, owner <> "/" <> repo]
             ++ ["-p" | private] -- creates private repository if asked so.
        -- Make a commit and push it.
        "git" ["add", "."]
        "git" ["commit", "-m", "Create the project"]
        "git" ["push", "-u", "origin", "master"]

    categoryText :: Text
    categoryText =
        [text|
        List of categories to choose from:

          * Control                    * Concurrency
          * Codec                      * Graphics
          * Data                       * Sound
          * Math                       * System
          * Parsing                    * Network
          * Text

          * Application                * Development
          * Compilers/Interpreters     * Testing
          * Web
          * Game
          * Utility

        |]
