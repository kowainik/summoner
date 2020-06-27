{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

{- |
Module                  : Summoner.CLI
Copyright               : (c) 2017-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module contains functions and data types to parse CLI inputs.
-}

module Summoner.CLI
       ( -- * CLI data types
         Command (..)
       , NewOpts (..)
       , ShowOpts (..)

         -- * Functions to parse CLI arguments and run @summoner@
       , summon
       , summonCli

         -- * Runners
       , runConfig
       , runScript

         -- * Common helper functions
       , getFinalConfig
       , getCustomLicenseText
       ) where

import Colourista (blue, bold, errorMessage, formatWith, infoMessage, successMessage,
                   warningMessage)
import Data.Version (Version, showVersion)
import Development.GitRev (gitCommitDate, gitHash)
import NeatInterpolation (text)
import Options.Applicative (Parser, ParserInfo, ParserPrefs, argument, command, customExecParser,
                            flag, fullDesc, help, helpLongEquals, helper, info, infoFooter,
                            infoHeader, infoOption, long, maybeReader, metavar, option, prefs,
                            progDesc, short, showDefault, showHelpOnEmpty, strArgument, strOption,
                            subparser, subparserInline, switch, value)
import Options.Applicative.Help.Chunk (stringChunk)
import Relude.Extra.Enum (universe)
import System.Directory (doesFileExist)
import System.Info (os)
import Validation (Validation (..))

import Summoner.Config (Config, ConfigP (..), PartialConfig, defaultConfig, finalise,
                        guessConfigFromGit, loadFileConfig)
import Summoner.CustomPrelude (CustomPrelude (..))
import Summoner.Decision (Decision (..))
import Summoner.Default (currentYear, defaultConfigFile, defaultConfigFileContent, defaultGHC)
import Summoner.GhcVer (GhcVer, ghcTable, parseGhcVer)
import Summoner.License (License (..), LicenseName (..), fetchLicenseCustom, parseLicenseName,
                         showLicenseWithDesc)
import Summoner.Mode (ConnectMode (..), Interactivity (..), isOffline)
import Summoner.Project (generateProject)
import Summoner.Settings (Tool, parseTool)
import Summoner.Template.Script (scriptFile)
import Summoner.Text (endLine)

import qualified Data.Text as T
import qualified Paths_summoner as Meta (version)


-- | Main function that parses @CLI@ commands and runs them using given
-- 'Command' handler.
summon :: Version -> (Command -> IO ()) -> IO ()
summon version performCommand =
    customExecParser summonerParserPrefs (cliParser version) >>= performCommand
  where
    -- To turn on some special options.
    summonerParserPrefs :: ParserPrefs
    summonerParserPrefs = prefs
        $ helpLongEquals
       <> showHelpOnEmpty
       <> subparserInline

-- | Runs @summoner@ in CLI mode.
summonCli :: IO ()
summonCli = summon Meta.version runCliCommand

-- | Run 'summoner' with @CLI@ command
runCliCommand :: Command -> IO ()
runCliCommand = \case
    New opts -> runNew opts
    Script opts -> runScript opts
    ShowInfo opts -> runShow opts
    Config opts -> runConfig opts


{- | Runs @config@ command

@
Usage: summon config [-f|--file=FILENAME]
  Create a default TOML configuration file for summoner

Available options:
  -h,--help                Show this help text
  -f,--file=FILENAME       Path to the toml file with configurations. If not
                           specified '~/.summoner.toml' will be used by default
@
-}
runConfig :: ConfigOpts -> IO ()
runConfig ConfigOpts{..} = do
    configFile <- whenNothing configOptsName defaultConfigFile
    let configFileTxt = toText configFile
    alreadyExist <- doesFileExist configFile
    if alreadyExist
    then do
        warningMessage $ "File '" <> configFileTxt <> "' already exits."
        infoMessage "Use 'summon config --file <path>' to specify another path."
        exitFailure
    else do
        writeFileText configFile defaultConfigFileContent
        infoMessage $ "Created default configuration file: " <> configFileTxt
        infoMessage "Open the file using the editor of your choice."

{- | Runs @show@ command.

@
Usage:
  summon show COMMAND
      Show supported licenses or ghc versions

Available commands:
  ghc                      Show available ghc versions
  license                  Show available licenses
  license [LICENSE_NAME]   Show specific license text
@

-}
runShow :: ShowOpts -> IO ()
runShow = \case
    -- show list of all available GHC versions
    GhcList -> showBulletList id ghcTable
    -- show a list of all available licenses
    LicenseList Nothing -> showBulletList @LicenseName showLicenseWithDesc universe
    -- show a specific license
    LicenseList (Just name) ->
        case parseLicenseName (toText name) of
            Nothing -> do
                errorMessage "This wasn't a valid choice."
                infoMessage "Here is the list of supported licenses:"
                showBulletList @LicenseName show universe
                -- get and show a license`s text
            Just licenseName -> do
                licenseCustomText <- getCustomLicenseText licenseName
                putTextLn $ unLicense licenseCustomText
  where
    showBulletList :: (a -> Text) -> [a] -> IO ()
    showBulletList showT = mapM_ (infoMessage . T.append "➤ " . showT)

-- | Get the customized License text for @summon show license NAME@ command.
getCustomLicenseText :: LicenseName -> IO License
getCustomLicenseText licenseName = do
    year <- currentYear
    guessConfig <- guessConfigFromGit
    fetchLicenseCustom
        licenseName
        (fromMaybe "YOUR NAME" $ getLast $ cFullName guessConfig)
        year

{- | Runs @script@ command.

@
Usage: summon script BUILD_TOOL (-g|--ghc GHC_VERSION) (-n|--name FILE_NAME)
  Create a new Haskell script

Available options:
  -h,--help                Show this help text
  -g,--ghc GHC_VERSION     Version of the compiler to be used for script
  -n,--name FILE_NAME      Name of the script file
@
-}
runScript :: ScriptOpts -> IO ()
runScript ScriptOpts{..} = do
    let pathTxt = toText scriptOptsName
    whenM (doesFileExist scriptOptsName) $ do
        errorMessage $ "File already exists: " <> pathTxt
        exitFailure

    let script = scriptFile scriptOptsGhc scriptOptsTool
    writeFileText scriptOptsName script
    unless (os == "mingw32") $
        "chmod" ["+x", pathTxt]
    successMessage $ "Successfully created script: " <> pathTxt

{- | Runs @new@ command.

@
Usage:
  summon new PROJECT_NAME [--ignore-config] [--no-upload] [--offline]
             [-f|--file FILENAME]
             [--cabal]
             [--stack]
             [--prelude-package PACKAGE_NAME]
             [--prelude-module MODULE_NAME]
             [with [OPTIONS]]
             [without [OPTIONS]]
@

-}
runNew :: NewOpts -> IO ()
runNew newOpts@NewOpts{..} = do
    -- get the final config
    finalConfig <- getFinalConfig newOpts
    -- Generate the project.
    generateProject newOptsInteractivity newOptsConnectMode newOptsProjectName finalConfig

-- | By the given 'NewOpts' return the final configurations.
getFinalConfig :: NewOpts -> IO Config
getFinalConfig NewOpts{..} = do
    -- read config from file
    fileConfig <- readFileConfig newOptsIgnoreFile newOptsConfigFile

    -- guess config from the global ~/.gitconfig file
    gitConfig <- guessConfigFromGit

    -- union all possible configs
    let unionConfig = defaultConfig <> gitConfig <> fileConfig <> newOptsCliConfig

    -- get the final config
    case finalise unionConfig of
        Success c    -> pure c
        Failure msgs -> for_ msgs errorMessage >> exitFailure

-- | Reads and parses the given config file. If no file is provided the default
-- configuration returned.
readFileConfig :: Bool -> Maybe FilePath -> IO PartialConfig
readFileConfig ignoreFile maybeFile = if ignoreFile then pure mempty else do
    (isDefault, file) <- case maybeFile of
        Nothing -> (True,) <$> defaultConfigFile
        Just x  -> pure (False, x)

    isFile <- doesFileExist file

    if isFile then do
        infoMessage $ "Configurations from " <> toText file <> " will be used."
        loadFileConfig file
    else if isDefault then do
        fp <- toText <$> defaultConfigFile
        warningMessage $ "Default config " <> fp <> " file is missing."
        pure mempty
    else do
        errorMessage $ "Specified configuration file " <> toText file <> " is not found."
        exitFailure

----------------------------------------------------------------------------
-- Command data types
----------------------------------------------------------------------------

-- | Represent all available commands
data Command
    -- | @new@ command creates a new project
    = New NewOpts
    -- | @script@ command creates Haskell script
    | Script ScriptOpts
    -- | @show@ command shows supported licenses or GHC versions
    | ShowInfo ShowOpts
    -- | @config@ command creates the TOML configuration file
    | Config ConfigOpts

-- | Options parsed with the @new@ command
data NewOpts = NewOpts
    { newOptsProjectName   :: !Text             -- ^ Project name
    , newOptsIgnoreFile    :: !Bool             -- ^ Ignore all config files if 'True'
    , newOptsConnectMode   :: !ConnectMode      -- ^ 'Online'/'Offline' mode
    , newOptsInteractivity :: !Interactivity    -- ^ Interactive or non-interactive mode is on?
    , newOptsConfigFile    :: !(Maybe FilePath) -- ^ File with custom configuration
    , newOptsCliConfig     :: !PartialConfig    -- ^ Config gathered via command-line
    }

-- | Options parsed with the @script@ command
data ScriptOpts = ScriptOpts
    { scriptOptsTool :: !Tool      -- ^ Build tool: `cabal` or `stack`
    , scriptOptsName :: !FilePath  -- ^ File path to the script
    , scriptOptsGhc  :: !GhcVer    -- ^ GHC version for this script
    }

-- | Commands parsed with @show@ command
data ShowOpts
    = GhcList
    | LicenseList (Maybe String)

-- | Options parsed with the @config@ command
newtype ConfigOpts = ConfigOpts
    { configOptsName :: Maybe FilePath
    }

----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

-- | Main parser of the app.
cliParser :: Version -> ParserInfo Command
cliParser version = modifyHeader
     $ modifyFooter
     $ info ( helper <*> versionP version <*> summonerP )
            $ fullDesc
           <> progDesc "Set up your own Haskell project"

versionP :: Version -> Parser (a -> a)
versionP version = infoOption (summonerVersion version)
    $ long "version"
   <> short 'v'
   <> help "Show summoner's version"

summonerVersion :: Version -> String
summonerVersion version = intercalate "\n" [sVersion, sHash, sDate]
  where
    sVersion, sHash, sDate :: String
    sVersion = formatWith [blue, bold] $ "Summoner " <> "v" <>  showVersion version
    sHash = " ➤ " <> formatWith [blue, bold] "Git revision: " <> $(gitHash)
    sDate = " ➤ " <> formatWith [blue, bold] "Commit date:  " <> $(gitCommitDate)

-- | All possible commands.
summonerP :: Parser Command
summonerP = subparser
    $ command "new" (info (helper <*> newP) $ progDesc "Create a new Haskell project")
   <> command "script" (info (helper <*> scriptP) $ progDesc "Create a new Haskell script")
   <> command "show" (info (helper <*> showP) $ progDesc "Show supported licenses or ghc versions")
   <> command "config" (info (helper <*> configP) $ progDesc "Create a default TOML configuration file for summoner")

----------------------------------------------------------------------------
-- @config@ command parsers
----------------------------------------------------------------------------

-- | Parses options of the @config@ command.
configP :: Parser Command
configP = do
    configOptsName <- optional configFileP
    pure $ Config ConfigOpts{..}

----------------------------------------------------------------------------
-- @show@ command parsers
----------------------------------------------------------------------------

-- | Parses options of the @show@ command.
showP :: Parser Command
showP = ShowInfo <$> subparser
    ( command "ghc" (info (helper <*> pure GhcList) $ progDesc "Show supported ghc versions")
   <> command "license" (info (helper <*> licenseText) $ progDesc "Show supported licenses")
    )

licenseText :: Parser ShowOpts
licenseText = LicenseList <$> optional
    (strArgument (metavar "LICENSE_NAME" <> help "Show specific license text"))

----------------------------------------------------------------------------
-- @script@ command parsers
----------------------------------------------------------------------------

-- | Parses options of the @script@ command.
scriptP :: Parser Command
scriptP = do
    scriptOptsTool <- toolArgP
    scriptOptsGhc  <- ghcVerP
    scriptOptsName <- strOption
         $ long "name"
        <> short 'n'
        <> value "my_script"
        <> metavar "FILE_NAME"
        <> help "Name of the script file"

    pure $ Script ScriptOpts{..}

-- | Argument parser for 'Tool'.
toolArgP :: Parser Tool
toolArgP = argument
    (maybeReader $ parseTool . toText)
    (metavar "BUILD_TOOL")

ghcVerP :: Parser GhcVer
ghcVerP = option
    (maybeReader $ parseGhcVer . toText)
    (  long "ghc"
    <> short 'g'
    <> value defaultGHC
    <> showDefault
    <> metavar "GHC_VERSION"
    <> help "Version of the compiler to be used for script"
    )

----------------------------------------------------------------------------
-- @new@ command parsers
----------------------------------------------------------------------------

-- | Parses options of the @new@ command.
newP :: Parser Command
newP = do
    newOptsProjectName   <- strArgument (metavar "PROJECT_NAME")
    newOptsIgnoreFile    <- ignoreFileP
    noUpload             <- noUploadP
    newOptsConnectMode   <- connectModeP
    newOptsInteractivity <- interactivityP
    newOptsConfigFile    <- optional configFileP
    cabal <- cabalP
    stack <- stackP
    preludePack <- optional preludePackP
    preludeMod  <- optional preludeModP
    with    <- optional withP
    without <- optional withoutP

    pure $ New $ NewOpts
        { newOptsCliConfig = (maybeToMonoid $ with <> without)
            { cPrelude = Last $ CustomPrelude <$> preludePack <*> preludeMod
            , cCabal = cabal
            , cStack = stack
            , cNoUpload = Any $ noUpload || isOffline newOptsConnectMode
            }
        , ..
        }

targetsP ::  Decision -> Parser PartialConfig
targetsP d = do
    cGitHub    <- githubP    d
    cGhActions <- ghActionsP d
    cTravis    <- travisP    d
    cAppVey    <- appVeyorP  d
    cPrivate   <- privateP   d
    cLib       <- libraryP   d
    cExe       <- execP      d
    cTest      <- testP      d
    cBench     <- benchmarkP d
    pure mempty
        { cGitHub    = cGitHub
        , cGhActions = cGhActions
        , cTravis    = cTravis
        , cAppVey    = cAppVey
        , cPrivate   = cPrivate
        , cLib       = cLib
        , cExe       = cExe
        , cTest      = cTest
        , cBench     = cBench
        }

githubP :: Decision -> Parser Decision
githubP d = flag Idk d $ mconcat
    [ long "github"
    , short 'g'
    , help "GitHub integration"
    ]

ghActionsP :: Decision -> Parser Decision
ghActionsP d = flag Idk d $ mconcat
    [ long "actions"
    , short 'a'
    , help "GitHub Actions CI"
    ]

travisP :: Decision -> Parser Decision
travisP d = flag Idk d $ mconcat
    [ long "travis"
    , short 'c'
    , help "Travis CI integration"
    ]

appVeyorP :: Decision -> Parser Decision
appVeyorP d = flag Idk d $ mconcat
    [ long "app-veyor"
    , short 'w'
    , help "AppVeyor CI integration"
    ]

privateP :: Decision -> Parser Decision
privateP d = flag Idk d $ mconcat
    [ long "private"
    , short 'p'
    , help "Private repository"
    ]

libraryP :: Decision -> Parser Decision
libraryP d = flag Idk d $ mconcat
    [ long "library"
    , short 'l'
    , help "Library folder"
    ]

execP :: Decision -> Parser Decision
execP d = flag Idk d $ mconcat
    [ long "exec"
    , short 'e'
    , help "Executable target"
    ]

testP :: Decision -> Parser Decision
testP d = flag Idk d $ mconcat
    [ long "test"
    , short 't'
    , help "Test target"
    ]

benchmarkP :: Decision -> Parser Decision
benchmarkP d = flag Idk d $ mconcat
    [ long "benchmark"
    , short 'b'
    , help "Benchmarks"
    ]

withP :: Parser PartialConfig
withP = subparser $ mconcat
    [ metavar "with [OPTIONS]"
    , command "with" $ info (helper <*> targetsP Yes) (progDesc "Specify options to enable")
    ]

withoutP :: Parser PartialConfig
withoutP = subparser $ mconcat
    [ metavar "without [OPTIONS]"
    , command "without" $ info (helper <*> targetsP Nop) (progDesc "Specify options to disable")
    ]

ignoreFileP :: Parser Bool
ignoreFileP = switch $ mconcat
    [ long "ignore-config"
    , help "Ignore configuration file"
    ]

noUploadP :: Parser Bool
noUploadP = switch $ mconcat
    [ long "no-upload"
    , help "Do not upload to GitHub. Special case of the '--offline' flag."
    ]

connectModeP :: Parser ConnectMode
connectModeP = flag Online Offline $ mconcat
    [ long "offline"
    , help "Offline mode: create a project with 'All Rights Reserved' license and without uploading to GitHub."
    ]

interactivityP :: Parser Interactivity
interactivityP = flag Interactive NonInteractive $ mconcat
    [ long "non-interactive"
    , short 'n'
    , help "Non-interactive mode: create a project without interactive questions."
    ]

configFileP :: Parser FilePath
configFileP = strOption $ mconcat
    [ long "file"
    , short 'f'
    , metavar "FILENAME"
    , help "Path to the toml file with configurations. If not specified '~/.summoner.toml' will be used by default"
    ]

preludePackP :: Parser Text
preludePackP = strOption $ mconcat
    [ long "prelude-package"
    , metavar "PACKAGE_NAME"
    , help "Name for the package of the custom prelude to use in the project"
    ]

preludeModP :: Parser Text
preludeModP = strOption $ mconcat
    [ long "prelude-module"
    , metavar "MODULE_NAME"
    , help "Name for the module of the custom prelude to use in the project"
    ]

cabalP :: Parser Decision
cabalP = flag Idk Yes $ mconcat
    [ long "cabal"
    , help "Cabal support for the project"
    ]

stackP :: Parser Decision
stackP = flag Idk Yes $ mconcat
    [ long "stack"
    , help "Stack support for the project"
    ]

----------------------------------------------------------------------------
-- Beauty util
----------------------------------------------------------------------------

-- to put custom header which doesn't cut all spaces
modifyHeader :: ParserInfo a -> ParserInfo a
modifyHeader p = p {infoHeader = stringChunk $ toString artHeader}

-- to put custom footer which doesn't cut all spaces
modifyFooter :: ParserInfo a -> ParserInfo a
modifyFooter p = p {infoFooter = stringChunk $ toString artFooter}

artHeader :: Text
artHeader = [text|
$endLine
                                                   ___
                                                 ╱  .  ╲
                                                │╲_/│   │
                                                │   │  ╱│
  ___________________________________________________-' │
 ╱                                                      │
╱   .-.                                                 │
│  ╱   ╲                                                │
│ │\_.  │ Summoner — tool for creating Haskell projects │
│\│  │ ╱│                                               │
│ `-_-' │                                              ╱
│       │_____________________________________________╱
│       │
 ╲     ╱
  `-_-'
|]

artFooter :: Text
artFooter = [text|
$endLine
              , *   +
           +      o   *             ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
            * @ ╭─╮  .      ________┃                                 ┃_______
           ╱| . │λ│ @   '   ╲       ┃   λ Haskell's summon scroll λ   ┃      ╱
         _╱ ╰─  ╰╥╯    O     ╲      ┃                                 ┃     ╱
        .─╲"╱. * ║  +        ╱      ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛     ╲
       ╱  ( ) ╲_ ║          ╱__________)                           (_________╲
       ╲ ╲(')╲__(╱
       ╱╱`)╱ `╮  ║
 `╲.  ╱╱  (   │  ║
  ╲.╲╱        │  ║
  `╰══════════╯
$endLine
|]
