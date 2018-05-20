{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE QuasiQuotes  #-}

-- | This module introduces functional for project creation.

module Summoner.Project
       ( Decision (..)
       , Targets (..)
       , generateProject
       ) where

import Control.Monad (when)
import Data.Aeson (decodeStrict)
import Data.ByteString.Char8 (pack)
import Data.List (nub)
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import NeatInterpolation (text)
import System.Info (os)
import System.Process (readProcess)

import Summoner.Ansi (successMessage, warningMessage)
import Summoner.Default (currentYear, defaultEmail, defaultGHC, defaultLicense, defaultName,
                         defaultOwner)
import Summoner.License (License (..), customizeLicense, githubLicenseQueryNames, licenseNames)
import Summoner.Process (deleteFile)
import Summoner.ProjectData (ProjectData (..))
import Summoner.Question (checkUniqueName, choose, query, queryDef)
import Summoner.Template (createStackTemplate)

import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Used for detecting the user decision during CLI input.
data Decision = Yes | Nop | Idk

instance Semigroup Decision where
    (<>) :: Decision -> Decision -> Decision
    Idk <> x   = x
    x   <> Idk = x
    _   <> x   = x

instance Monoid Decision where
    mempty  = Idk
    mappend = (<>)

data Targets = Targets
    { githubFlag   :: Decision
    , travisFlag   :: Decision
    , appVeyorFlag :: Decision
    , privateFlag  :: Decision
    , scriptFlag   :: Decision
    , isLibrary    :: Decision
    , isExecutable :: Decision
    , isTest       :: Decision
    , isBenchmark  :: Decision
    }

instance Semigroup Targets where
    t1 <> t2 = Targets
        { githubFlag   = combine githubFlag
        , travisFlag   = combine travisFlag
        , appVeyorFlag = combine appVeyorFlag
        , privateFlag  = combine privateFlag
        , scriptFlag   = combine scriptFlag
        , isLibrary    = combine isLibrary
        , isExecutable = combine isExecutable
        , isTest       = combine isTest
        , isBenchmark  = combine isBenchmark
        }
      where
        combine field = field t1 <> field t2

instance Monoid Targets where
    mempty  = Targets mempty mempty mempty mempty mempty mempty mempty mempty mempty
    mappend = (<>)


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
generateProject :: Text -> Targets -> IO ()
generateProject projectName Targets{..} = do
    repo        <- checkUniqueName projectName
    owner       <- queryDef "Repository owner: " defaultOwner
    description <- query "Short project description: "
    nm          <- queryDef "Author: " defaultName
    email       <- queryDef "Maintainer e-mail: " defaultEmail
    T.putStr
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
    category <- query "Category: "
    license  <- choose "License: " $ nub (defaultLicense : licenseNames)

    -- License creation
    let licenseGithub = snd
                      $ head
                      $ dropWhile ((/= license) . fst) githubLicenseQueryNames
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
            Just t  -> customizeLicense license (lcnsText t) nm year
            Nothing -> error "Broken predefined license list"

    -- Library/Executable/Tests/Benchmarks flags
    github <- decisionToBool githubFlag "github integration"
    travis <- ifGithub github "Travis CI integration" travisFlag
    appVey <- ifGithub github "AppVeyor CI integration" appVeyorFlag
    privat <- ifGithub github "Private repository" privateFlag
    script <- decisionToBool scriptFlag "build script"
    isLib  <- decisionToBool isLibrary "library target"
    isExe  <- let target = "executable target" in
              if isLib
              then decisionToBool isExecutable target
              else trueMessage target
    test   <- decisionToBool isTest "tests"
    bench  <- decisionToBool isBenchmark "benchmarks"

    putStrLn "Latest GHCs: 7.10.3 8.0.2 8.2.2"
    putStrLn $ "The project will be created with the latest resolver for GHC-" ++ T.unpack defaultGHC
    testedVersions <- T.words <$>
      queryDef "Additionally you can specify versions of GHC to test with (space-separated): " ""
    let prData = ProjectData{..}
    -- create stack project
    doStackCommands prData
    -- make b executable
    when script doScriptCommand
    -- create github repository and commit
    when github $ doGithubCommands prData privat

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
