{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

-- | This module contains functions and data types to parse CLI inputs.

module Summoner.CLI
       ( summon
       ) where

import Relude
import Relude.Extra.Enum (universe)

import Data.Aeson (decodeStrict)
import Data.ByteString.Char8 (pack)
import Data.Version (showVersion)
import Development.GitRev (gitCommitDate, gitDirty, gitHash)
import NeatInterpolation (text)
import Options.Applicative (Parser, ParserInfo, command, execParser, flag, flag', fullDesc, help,
                            helper, info, infoFooter, infoHeader, infoOption, long, metavar,
                            optional, progDesc, short, strArgument, strOption, subparser, switch)
import Options.Applicative.Help.Chunk (stringChunk)
import System.Directory (doesFileExist)
import System.Process (readProcess)

import Paths_summoner (version)
import Summoner.Ansi (Color (Green), beautyPrint, blueCode, bold, boldCode, errorMessage,
                      infoMessage, redCode, resetCode, setColor, warningMessage)
import Summoner.Config (ConfigP (..), PartialConfig, defaultConfig, finalise, loadFileConfig)
import Summoner.Decision (Decision (..))
import Summoner.Default (defaultConfigFile, endLine)
import Summoner.GhcVer (showGhcVer)
import Summoner.License (License (..), LicenseName (..), githubLicenseQueryNames, parseLicenseName)
import Summoner.Project (generateProject)
import Summoner.ProjectData (CustomPrelude (..))
import Summoner.Validation (Validation (..))

---------------------------------------------------------------------------
-- CLI
----------------------------------------------------------------------------

summon :: IO ()
summon = execParser prsr >>= runCommand

-- | Run 'summoner' with cli command
runCommand :: Command -> IO ()
runCommand = \case
    New opts -> runNew opts
    ShowInfo opts -> runShow opts

runShow :: ShowOpts -> IO ()
runShow opts =
    case opts of
        -- show list of all available GHC versions
        ShowGhc          -> putText $ unlines $ map showGhcVer universe
        ShowLicense name -> case name of
            -- show a list of all available licenses
            LicenseList Nothing  -> putText $ unlines $ map (\x -> show (x :: LicenseName)) universe
            -- show a specific license
            LicenseList (Just n) -> do
                -- check a user`s input
                license <- whenNothing (parseLicenseName (toText n))
                    (fail "This wasn't a valid choice.")
                -- get a link to github
                let licenseLink = "https://api.github.com/licenses/" <> githubLicenseQueryNames license
                -- get a JSON of specific license from github
                licenseJson <-
                    readProcess "curl"
                                [ toString licenseLink
                                , "-H"
                                , "Accept: application/vnd.github.drax-preview+json"
                                ]
                                ""
                -- get a license`s text and show
                case (decodeStrict $ pack licenseJson) :: Maybe License of
                    Just t  -> putStr $ unLicense t
                    Nothing -> error "Broken predefined license list"

runNew :: NewOpts -> IO ()
runNew NewOpts{..} = do
    -- read config from file
    fileConfig <- readFileConfig ignoreFile maybeFile

    -- union all possible configs
    let unionConfig = defaultConfig <> fileConfig <> cliConfig

    -- get the final config
    finalConfig <- case finalise unionConfig of
             Success c    -> pure c
             Failure msgs -> do
                 for_ msgs errorMessage
                 exitFailure

    -- Generate the project.
    generateProject projectName finalConfig

    -- print result
    beautyPrint [bold, setColor Green] "\nJob's done\n"

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
    -- | @show@ command shows available licenses or GHC versions
    | ShowInfo ShowOpts

-- | Options parsed with @new@ command
data NewOpts = NewOpts
    { projectName :: Text           -- ^ project name
    , ignoreFile  :: Bool           -- ^ ignore all config files if 'True'
    , maybeFile   :: Maybe FilePath -- ^ file with custom configuration
    , cliConfig   :: PartialConfig  -- ^ config gathered during CLI
    }

data ShowOpts = ShowGhc | ShowLicense LicenseList

newtype LicenseList = LicenseList (Maybe String)

----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

-- | Main parser of the app.
prsr :: ParserInfo Command
prsr = modifyHeader
     $ modifyFooter
     $ info ( helper <*> versionP <*> summonerP )
            $ fullDesc
           <> progDesc "Set up your own Haskell project"

versionP :: Parser (a -> a)
versionP = infoOption summonerVersion
    $ long "version"
   <> short 'v'
   <> help "Show summoner's version"

summonerVersion :: String
summonerVersion = toString $ intercalate "\n" $ [sVersion, sHash, sDate] ++ [sDirty | $(gitDirty)]
  where
    sVersion = blueCode <> boldCode <> "Summoner " <> "v" <>  showVersion version <> resetCode
    sHash = " ➤ " <> blueCode <> boldCode <> "Git revision: " <> resetCode <> $(gitHash)
    sDate = " ➤ " <> blueCode <> boldCode <> "Commit date:  " <> resetCode <> $(gitCommitDate)
    sDirty = redCode <> "There are non-committed files." <> resetCode

-- All possible commands.
summonerP :: Parser Command
summonerP = subparser
    $ command "new" (info (helper <*> newP) $ progDesc "Create a new Haskell project")
   <> command "show" (info (helper <*> showP) $ progDesc "Show available licenses or ghc versions")

----------------------------------------------------------------------------
-- New command parsers
----------------------------------------------------------------------------

-- | Parses options of the @show@ command.
showP :: Parser Command
showP = ShowInfo <$> (ghcP <|> ShowLicense <$> licenseP)

ghcP :: Parser ShowOpts
ghcP = flag' ShowGhc $ long "ghc" <> short 'g' <> help "Show available ghc versions"

licenseP :: Parser LicenseList
licenseP = subparser $
    command "licenses" (info (helper <*> licenseList) $ progDesc "Show available licenses")

licenseList :: Parser LicenseList
licenseList =  LicenseList <$> optional licenseText

licenseText :: Parser String
licenseText = strOption $
    long "name" <> short 'n' <> metavar "LICENSE_NAME" <> help "Show license text"

-- | Parses options of the @new@ command.
newP :: Parser Command
newP = do
    projectName <- strArgument (metavar "PROJECT_NAME")
    ignoreFile  <- ignoreFileP
    cabal   <- cabalP
    stack   <- stackP
    with    <- optional withP
    without <- optional withoutP
    file    <- optional fileP
    preludePack <- optional preludePackP
    preludeMod  <- optional preludeModP

    pure $ New $ NewOpts projectName ignoreFile file
        $ (maybeToMonoid $ with <> without)
            { cPrelude = Last $ Prelude <$> preludePack <*> preludeMod
            , cCabal = cabal
            , cStack = stack
            }

targetsP ::  Decision -> Parser PartialConfig
targetsP d = do
    cGitHub  <- githubP    d
    cTravis  <- travisP    d
    cAppVey  <- appVeyorP  d
    cPrivate <- privateP   d
    cScript  <- scriptP    d
    cLib     <- libraryP   d
    cExe     <- execP      d
    cTest    <- testP      d
    cBench   <- benchmarkP d
    pure mempty
        { cGitHub = cGitHub
        , cTravis = cTravis
        , cAppVey = cAppVey
        , cPrivate= cPrivate
        , cScript = cScript
        , cLib    = cLib
        , cExe    = cExe
        , cTest   = cTest
        , cBench  = cBench
        }

githubP :: Decision -> Parser Decision
githubP d = flag Idk d
          $ long "github"
         <> short 'g'
         <> help "GitHub integration"

travisP :: Decision -> Parser Decision
travisP d = flag Idk d
          $ long "travis"
         <> short 'c'
         <> help "Travis CI integration"

appVeyorP :: Decision -> Parser Decision
appVeyorP d = flag Idk d
            $ long "app-veyor"
           <> short 'w'
           <> help "AppVeyor CI integration"

privateP :: Decision -> Parser Decision
privateP d = flag Idk d
           $ long "private"
          <> short 'p'
          <> help "Private repository"

scriptP :: Decision -> Parser Decision
scriptP d = flag Idk d
          $ long "script"
         <> short 's'
         <> help "Build script for convenience"

libraryP :: Decision -> Parser Decision
libraryP d = flag Idk d
           $ long "library"
          <> short 'l'
          <> help "Library folder"

execP :: Decision -> Parser Decision
execP d = flag Idk d
        $ long "exec"
       <> short 'e'
       <> help "Executable target"

testP :: Decision -> Parser Decision
testP d = flag Idk d
        $ long "test"
       <> short 't'
       <> help "Test target"

benchmarkP :: Decision -> Parser Decision
benchmarkP d = flag Idk d
             $ long "benchmark"
            <> short 'b'
            <> help "Benchmarks"

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
ignoreFileP = switch $ long "ignore-config" <> help "Ignore configuration file"

fileP :: Parser FilePath
fileP = strOption
    $ long "file"
   <> short 'f'
   <> metavar "FILENAME"
   <> help "Path to the toml file with configurations. If not specified '~/.summoner.toml' will be used if present"

preludePackP :: Parser Text
preludePackP = strOption
    $ long "prelude-package"
   <> metavar "PACKAGE_NAME"
   <> help "Name for the package of the custom prelude to use in the project"

preludeModP :: Parser Text
preludeModP = strOption
    $ long "prelude-module"
   <> metavar "MODULE_NAME"
   <> help "Name for the module of the custom prelude to use in the project"

cabalP :: Parser Decision
cabalP = flag Idk Yes
       $ long "cabal"
      <> help "Cabal support for the project"

stackP :: Parser Decision
stackP = flag Idk Yes
       $ long "stack"
      <> help "Stack support for the project"

-- showGhc :: Parser Command
-- showGhc = subparser Info $

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
