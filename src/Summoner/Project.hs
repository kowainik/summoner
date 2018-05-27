{-# LANGUAGE QuasiQuotes #-}

-- | This module introduces functional for project creation.

module Summoner.Project
       ( generateProject
       ) where

import Data.Aeson (decodeStrict)
import Data.ByteString.Char8 (pack)
import NeatInterpolation (text)
import System.Info (os)
import System.Process (readProcess)

import Summoner.Ansi (Color (..), beautyPrint, bold, infoMessage, italic, setColor, skipMessage,
                      successMessage, warningMessage)
import Summoner.Config (Config, ConfigP (..))
import Summoner.Default (currentYear, defaultGHC)
import Summoner.License (License (..), customizeLicense, githubLicenseQueryNames, licenseNames)
import Summoner.Process ()
import Summoner.ProjectData (CustomPrelude (..), Decision (..), ProjectData (..), parseGhcVer,
                             showGhcVer, supportedGhcVers)
import Summoner.Question (checkUniqueName, choose, query, queryDef, queryManyRepeatOnFail)
import Summoner.Template (createStackTemplate)
import Summoner.Tree (traverseTree)

import qualified Universum.Unsafe as Unsafe

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

targetMessage :: Bool -> Text -> IO Bool
targetMessage result target = do
    let (color, actionResult) = case result of
          False -> (Cyan,  " won't be added to the project")
          True  -> (Green, " will be added to the project")

    beautyPrint [italic, bold, setColor color] $ "  " <> target
    beautyPrint [setColor color] actionResult
    putTextLn ""

    pure result

trueMessage, falseMessage :: Text -> IO Bool
trueMessage  = targetMessage True
falseMessage = targetMessage False

-- | Generate the project.
generateProject :: Text -> Config -> IO ()
generateProject projectName Config{..} = do
    repo        <- checkUniqueName projectName
    owner       <- queryDef "Repository owner: " cOwner
    description <- query "Short project description: "
    nm          <- queryDef "Author: " cFullName
    email       <- queryDef "Maintainer e-mail: " cEmail
    putText categoryText
    category <- query "Category: "
    license  <- choose "License: " $ map unLicense $ ordNub (cLicense : licenseNames)

    -- License creation
    let licenseGithub = snd
                      $ Unsafe.head
                      $ dropWhile ((/= license) . unLicense . fst) githubLicenseQueryNames
    let licenseLink = "https://api.github.com/licenses/" <> licenseGithub
    licenseJson <-
      readProcess "curl"
                  [ toString licenseLink
                  , "-H"
                  , "Accept: application/vnd.github.drax-preview+json"
                  ]
                  ""
    year <- currentYear
    let licenseText = case (decodeStrict $ pack licenseJson) :: Maybe License of
            Just t  -> customizeLicense license (unLicense t) nm year
            Nothing -> error "Broken predefined license list"

    -- Library/Executable/Tests/Benchmarks flags
    github <- decisionToBool cGitHub "GitHub integration"
    travis <- ifGithub github "Travis CI integration" cTravis
    appVey <- ifGithub github "AppVeyor CI integration" cAppVey
    privat <- ifGithub github "Private repository" cPrivate
    script <- decisionToBool cScript "Build script"
    isLib  <- decisionToBool cLib "Library target"
    isExe  <- let target = "Executable target" in
              if isLib
              then decisionToBool cExe target
              else trueMessage target
    test   <- decisionToBool cTest "Tests"
    bench  <- decisionToBool cBench "Benchmarks"
    prelude <- if isLib then getPrelude else pure Nothing
    let base = case prelude of
            Nothing -> "base"
            Just _  -> "base-noprelude"

    let extensions = cExtensions

    putTextLn $ "The project will be created with the latest resolver for default GHC-" <> showGhcVer defaultGHC
    testedVersions <- (sortNub . (defaultGHC :)) <$> case cGhcVer of
        [] -> do
            putTextLn "Additionally you can specify versions of GHC to test with (space-separated): "
            infoMessage $ "Supported by 'summoner' GHCs: " <> intercalateMap " " showGhcVer supportedGhcVers
            queryManyRepeatOnFail parseGhcVer
        vers -> do
            putTextLn $ "Also these GHC versions will be added: " <> intercalateMap " " showGhcVer vers
            pure vers

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
        traverseTree $ createStackTemplate projectData
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

    getPrelude :: IO (Maybe CustomPrelude)
    getPrelude = case (cPreludePackage, cPreludeModule) of
        (Last Nothing, Last Nothing) -> do
            ch <- choose "Add custom prelude?" ["y", "n"]
            case ch of
                "y" -> do
                    p <- query "Custom prelude package: "
                    m <- query "Custom prelude module: "
                    pure $ Just $ Prelude p m
                "n" -> Nothing <$ skipMessage "Custom prelude won't be added to the project"
                _   -> error "Impossible happened"
        (Last Nothing, Last (Just m)) -> do
            warningMessage $ "Prelude is not specified for " <> m <> " module. Base prelude will be used"
            pure Nothing
        (Last (Just p), Last Nothing) -> do
            warningMessage $ "Module is not specified for " <> p <> ". Base prelude will be used"
            pure Nothing
        (Last (Just p), Last (Just m)) -> do
            successMessage $ "Custom prelude " <> p <> " will be used in the project"
            pure $ Just $ Prelude p m
