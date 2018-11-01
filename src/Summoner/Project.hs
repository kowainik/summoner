{-# LANGUAGE QuasiQuotes #-}

-- | This module introduces functional for project creation.

module Summoner.Project
       ( generateProject
       ) where

import NeatInterpolation (text)
import System.Directory (setCurrentDirectory)

import Summoner.Ansi (errorMessage, infoMessage, successMessage)
import Summoner.Config (Config, ConfigP (..))
import Summoner.Decision (Decision (..), decisionToBool)
import Summoner.Default (currentYear, defaultGHC)
import Summoner.GhcVer (parseGhcVer, showGhcVer)
import Summoner.License (customizeLicense, fetchLicense, parseLicenseName)
import Summoner.Process ()
import Summoner.Question (checkUniqueName, choose, chooseYesNo, falseMessage, query, queryDef,
                          queryManyRepeatOnFail, targetMessageWithText, trueMessage)
import Summoner.Settings (CustomPrelude (..), Settings (..))
import Summoner.Source (fetchSource)
import Summoner.Template (createProjectTemplate)
import Summoner.Text (intercalateMap, packageToModule)
import Summoner.Tree (showTree, traverseTree)

-- | Generate the project.
generateProject :: Bool -> Text -> Config -> IO ()
generateProject noUpload projectName Config{..} = do
    settingsRepo   <- checkUniqueName projectName
    -- decide cabal stack or both
    (settingsCabal, settingsStack) <- getCabalStack (cCabal, cStack)

    settingsOwner       <- queryDef "Repository owner: " cOwner
    settingsDescription <- query "Short project description: "
    settingsFullName    <- queryDef "Author: " cFullName
    settingsEmail       <- queryDef "Maintainer e-mail: " cEmail

    putText categoryText
    settingsCategories <- query "Category: "

    settingsLicenseName  <- choose parseLicenseName "License: " $ ordNub (cLicense : universe)

    -- License creation
    fetchedLicense <- fetchLicense settingsLicenseName
    settingsYear <- currentYear
    let settingsLicenseText = customizeLicense  -- TODO: use named here as well
            settingsLicenseName
            fetchedLicense
            settingsFullName
            settingsYear

    -- Library/Executable/Tests/Benchmarks flags
    settingsGithub <- decisionToBool cGitHub "GitHub integration"
    settingsTravis <- ifGithub settingsGithub "Travis CI integration" cTravis
    settingsAppVey <- ifGithub (settingsStack && settingsGithub) "AppVeyor CI integration" cAppVey
    settingsPrivat <- ifGithub settingsGithub "private repository" $ if noUpload then Nop else cPrivate
    settingsIsLib  <- decisionToBool cLib "library target"
    settingsIsExe  <- let target = "executable target" in
              if settingsIsLib
              then decisionToBool cExe target
              else trueMessage target
    settingsTest   <- decisionToBool cTest "tests"
    settingsBench  <- decisionToBool cBench "benchmarks"
    settingsPrelude <- if settingsIsLib then getPrelude else pure Nothing
    let settingsBase = case settingsPrelude of
            Nothing -> "base"
            Just _  -> "base-noprelude"

    let settingsExtensions = cExtensions
    let settingsWarnings = cWarnings

    putTextLn $ "The project will be created with the latest resolver for default GHC-" <> showGhcVer defaultGHC
    settingsTestedVersions <- sortNub . (defaultGHC :) <$> case cGhcVer of
        [] -> do
            putTextLn "Additionally you can specify versions of GHC to test with (space-separated): "
            infoMessage $ "Supported by 'summoner' GHCs: " <> intercalateMap " " showGhcVer universe
            queryManyRepeatOnFail parseGhcVer
        vers -> do
            putTextLn $ "Also these GHC versions will be added: " <> intercalateMap " " showGhcVer vers
            pure vers

    let fetchLast = maybe (pure Nothing) fetchSource . getLast
    settingsStylish      <- fetchLast cStylish
    settingsContributing <- fetchLast cContributing

    -- Create project data from all variables in scope
    let settings = Settings{..}

    createProjectDirectory settings
    -- Create github repository, commit, optionally push and make it private 
    when settingsGithub $ doGithubCommands settings settingsPrivat

 where
    ifGithub :: Bool -> Text -> Decision -> IO Bool
    ifGithub github target decision = if github
        then decisionToBool decision target
        else falseMessage target

    createProjectDirectory :: Settings -> IO ()
    createProjectDirectory settings@Settings{..} = do
        let tree = createProjectTemplate settings
        traverseTree tree
        successMessage "\nThe project with the following structure has been created:"
        putTextLn $ showTree tree
        setCurrentDirectory (toString settingsRepo)

    doGithubCommands :: Settings -> Bool -> IO ()
    doGithubCommands Settings{..} private = do
        -- Create git repostitory and do a commit.
        "git" ["init"]
        "git" ["add", "."]
        "git" ["commit", "-m", "Create the project"]
        unless noUpload ( do
             -- Create private repository
            "hub" $ ["create", "-d", settingsDescription, settingsOwner <> "/" <> settingsRepo]
                    ++ ["-p" | private] 
             -- Upload repository to GitHub.
            "git" ["push", "-u", "origin", "master"])
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
    getPrelude = case cPrelude of
        Last Nothing -> do
            let yesDo, noDo :: IO (Maybe CustomPrelude)
                yesDo = do
                    p <- query "Custom prelude package: "
                    m <- queryDef "Custom prelude module: " (packageToModule p)
                    successMessage $ "Custom prelude " <> p <> " will be used in the project"
                    pure $ Just $ CustomPrelude p m
                noDo = pure Nothing
            chooseYesNo "custom prelude" yesDo noDo
        Last prelude@(Just (CustomPrelude p _)) ->
            prelude <$ successMessage ("Custom prelude " <> p <> " will be used in the project")

    -- get what build tool to use in the project
    -- If user chose only one during CLI, we assume to use only that one.
    getCabalStack :: (Decision, Decision) -> IO (Bool, Bool)
    getCabalStack = \case
        (Idk, Idk) -> decisionToBool cCabal "cabal" >>= \c ->
            if c then decisionToBool cStack "stack" >>= \s -> pure (c, s)
            else stackMsg True >> pure (False, True)
        (Nop, Nop) -> errorMessage "Neither cabal nor stack was chosen" >> exitFailure
        (Yes, Yes) -> output (True, True)
        (Yes, _)   -> output (True, False)
        (_, Yes)   -> output (False, True)
        (Nop, Idk) -> output (False, True)
        (Idk, Nop) -> output (True, False)
      where
        output :: (Bool, Bool) -> IO (Bool, Bool)
        output x@(c, s) = cabalMsg c >> stackMsg s >> pure x

        cabalMsg c = targetMessageWithText c "Cabal" "used in this project"
        stackMsg c = targetMessageWithText c "Stack" "used in this project"
