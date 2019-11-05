{-# LANGUAGE QuasiQuotes #-}

-- | This module introduces functions for the project creation.

module Summoner.Project
       ( generateProject
       , initializeProject
       , fetchSources
       ) where

import Data.List (intersect)
import NeatInterpolation (text)
import Shellmet ()
import System.Directory (setCurrentDirectory)

import Summoner.Ansi (Color (Green), beautyPrint, bold, errorMessage, infoMessage, setColor,
                      skipMessage, successMessage, warningMessage)
import Summoner.Config (Config, ConfigP (..))
import Summoner.CustomPrelude (CustomPrelude (..))
import Summoner.Decision (Decision (..), decisionToBool)
import Summoner.Default (currentYear, defaultDescription, defaultGHC)
import Summoner.GhcVer (oldGhcs, parseGhcVer, showGhcVer)
import Summoner.License (LicenseName (..), customizeLicense, fetchLicense, licenseShortDesc,
                         parseLicenseName)
import Summoner.Question (YesNoPrompt (..), checkUniqueName, choose, falseMessage,
                          mkDefaultYesNoPrompt, query, queryDef, queryManyRepeatOnFail,
                          queryWithPredicate, targetMessageWithText, trueMessage)
import Summoner.Settings (Settings (..))
import Summoner.Source (Source, fetchSource)
import Summoner.Template (createProjectTemplate)
import Summoner.Text (intercalateMap, moduleNameValid, packageNameValid, packageToModule)
import Summoner.Tree (TreeFs, pathToTree, showBoldTree, traverseTree)

import qualified Data.Map.Strict as Map


-- | Generate the project.
generateProject
    :: Bool        -- ^ @offline@ mode option
    -> Text        -- ^ Given project name.
    -> Config      -- ^ Given configurations.
    -> IO ()
generateProject isOffline projectName Config{..} = do
    unless (null cWarnings) $
        warningMessage "Please, rename 'warnings' field if you use one, it will be removed in the very next release. Use 'ghc-options' instead."

    settingsRepo <- checkUniqueName projectName
    -- decide cabal stack or both
    (settingsCabal, settingsStack) <- getCabalStack (cCabal, cStack)

    settingsOwner       <- queryDef "Repository owner: " cOwner
    settingsDescription <- queryDef "Short project description: " defaultDescription
    settingsFullName    <- queryDef "Author: " cFullName
    settingsEmail       <- queryDef "Maintainer e-mail: " cEmail

    putText categoryText
    settingsCategories <- query "Category: "

    putText licenseText
    settingsLicenseName  <- if isOffline
        then None <$ infoMessage "'AllRightsReserved' license is used in offline mode"
        else choose parseLicenseName "License: " $ ordNub (cLicense : universe)

    -- License creation
    fetchedLicense <- fetchLicense settingsLicenseName
    settingsYear <- currentYear
    let settingsLicenseText = customizeLicense
            settingsLicenseName
            fetchedLicense
            settingsFullName
            settingsYear

    settingsGitHub   <- decisionToBool cGitHub
        (YesNoPrompt "GitHub integration" "Do you want to create a GitHub repository?")

    let settingsNoUpload = getAny cNoUpload
    when settingsNoUpload $ do
        infoMessage "'No upload' option is selected. The project won't be uploaded to GitHub."
        infoMessage "Use 'hub' and 'git' commands manually in order to upload the project to GitHub"
    settingsPrivate  <- decisionIf
        (settingsGitHub && not settingsNoUpload)
        (YesNoPrompt "private repository" "Create as a private repository (Requires a GitHub private repo plan)?")
        cPrivate
    settingsTravis   <- decisionIf settingsGitHub (mkDefaultYesNoPrompt "Travis CI integration") cTravis
    settingsAppVeyor <- decisionIf settingsGitHub (mkDefaultYesNoPrompt "AppVeyor CI integration") cAppVey
    settingsIsLib    <- decisionToBool cLib (mkDefaultYesNoPrompt "library target")
    settingsIsExe    <- let target = "executable target" in
        if settingsIsLib
        then decisionToBool cExe (mkDefaultYesNoPrompt target)
        else trueMessage target
    settingsTest     <- decisionToBool cTest (mkDefaultYesNoPrompt "tests")
    settingsBench    <- decisionToBool cBench (mkDefaultYesNoPrompt "benchmarks")
    settingsPrelude  <- if settingsIsLib then getPrelude else pure Nothing
    let settingsBaseType = case settingsPrelude of
            Nothing -> "base"
            Just _  -> "base-noprelude"

    let settingsExtensions = cExtensions
    let settingsGhcOptions = cWarnings ++ cGhcOptions
    let settingsGitignore = cGitignore


    putTextLn $ "The project will be created with GHC-" <> showGhcVer defaultGHC
    settingsTestedVersions <- sortNub . (defaultGHC :) <$> case cGhcVer of
        [] -> do
            putTextLn "Additionally you can specify versions of GHC to test with (space-separated): "
            infoMessage $ "Supported by 'summoner' GHCs: " <> intercalateMap " " showGhcVer universe
            queryManyRepeatOnFail parseGhcVer
        vers -> do
            putTextLn $ "Also these GHC versions will be added: " <> intercalateMap " " showGhcVer vers
            pure vers
    -- Inform if there are old GHCs that won't be included to Travis Stack matrix
    let oldGhcIncluded = not $ null $ settingsTestedVersions `intersect` oldGhcs
    when (oldGhcIncluded && settingsStack && settingsTravis) $
        warningMessage "Old GHC versions won't be included into Stack matrix at Travis CI because of the Stack issue with newer Cabal versions."

    let fetchLast :: Text -> Last Source -> IO (Maybe Text)
        fetchLast option (Last mSource) = case mSource of
            Nothing -> pure Nothing
            Just source -> do
                let msg = [text|The option '${option}' is deprecated. Use 'files' instead.|]
                warningMessage msg
                fetchSource isOffline source

    settingsStylish      <- fetchLast "stylish.{url,file,link}" cStylish
    settingsContributing <- fetchLast "contributing.{url,file,link}" cContributing
    settingsFiles <- fetchSources isOffline cFiles

    -- Create project data from all variables in scope
    -- and make a project from it.
    initializeProject Settings{..}
 where
    decisionIf :: Bool -> YesNoPrompt -> Decision -> IO Bool
    decisionIf p ynPrompt decision = if p
        then decisionToBool decision ynPrompt
        else falseMessage (yesNoTarget ynPrompt)


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

    licenseText :: Text
    licenseText = "List of licenses to choose from:\n\n"
        <> unlines (map showShort $ universe @LicenseName)
        <> "\n"
      where
        showShort :: LicenseName -> Text
        showShort l = "  * " <> show l <> ": " <> licenseShortDesc l

    getPrelude :: IO (Maybe CustomPrelude)
    getPrelude = case cPrelude of
        Last Nothing -> do
            p <- queryWithPredicate
                "Custom prelude package (leave empty if no custom prelude is needed): "
                []
                "Name can contain letters/numbers/'-'"
                packageNameValid
            if p == "" then Nothing <$ skipMessage "No custom prelude will be used in the project"
            else do
                let defModule = packageToModule p
                input <- queryWithPredicate
                    "Custom prelude module: "
                    [defModule]
                    "Name can contain dot-separated capitalized letter/numeral fragments. Ex: This.Is.Valid1"
                    moduleNameValid
                let m = if input == "" then defModule else input
                successMessage $ "Custom prelude " <> p <> " will be used in the project"
                pure $ Just $ CustomPrelude p m
        Last prelude@(Just (CustomPrelude p _)) ->
            prelude <$ successMessage ("Custom prelude " <> p <> " will be used in the project")

    -- get what build tool to use in the project
    -- If user chose only one during CLI, we assume to use only that one.
    getCabalStack :: (Decision, Decision) -> IO (Bool, Bool)
    getCabalStack = \case
        (Idk, Idk) -> decisionToBool cCabal (mkDefaultYesNoPrompt "cabal") >>= \c ->
            if c then decisionToBool cStack (mkDefaultYesNoPrompt "stack") >>= \s -> pure (c, s)
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

-- | Creates the directory and run GitHub commands.
initializeProject :: Settings -> IO ()
initializeProject settings@Settings{..} = do
    createProjectDirectory settings
    when settingsGitHub $ doGithubCommands settings
    beautyPrint [bold, setColor Green] "\nJob's done\n"

{- | This function fetches contents of extra file sources.
-}
fetchSources :: Bool -> Map FilePath Source -> IO [TreeFs]
fetchSources isOffline = mapMaybeM sourceToTree . Map.toList
  where
    sourceToTree :: (FilePath, Source) -> IO (Maybe TreeFs)
    sourceToTree (path, source) = do
        infoMessage $ "Fetching content of the extra file: " <> toText path
        fetchSource isOffline source >>= \case
            Nothing -> do
                errorMessage $ "Error fetching: " <> toText path
                pure Nothing
            Just content -> pure $ Just $ pathToTree path content

-- | From the given 'Settings' creates the project.
createProjectDirectory :: Settings -> IO ()
createProjectDirectory settings@Settings{..} = do
    let tree = createProjectTemplate settings
    traverseTree tree
    successMessage "\nThe project with the following structure has been created:"
    putTextLn $ showBoldTree tree
    setCurrentDirectory (toString settingsRepo)

-- | Init, commit and push repository to GitHub.
doGithubCommands :: Settings -> IO ()
doGithubCommands Settings{..} = do
    -- Create git repostitory and do a commit.
    "git" ["init"]
    "git" ["add", "."]
    "git" ["commit", "-m", "Create the project"]
    unless settingsNoUpload $ do
        "hub" $ ["create", "-d", settingsDescription, settingsOwner <> "/" <> settingsRepo]
             ++ ["-p" | settingsPrivate]  -- Create private repository if asked so
         -- Upload repository to GitHub.
        "git" ["push", "-u", "origin", "master"]
