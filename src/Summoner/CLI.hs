{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | This module contains functions and data types to parse CLI inputs.

module Summoner.CLI
       ( summon
       ) where

import Data.Foldable (fold)
import Data.Semigroup (Semigroup (..))
import Data.Text (Text)
import NeatInterpolation (text)
import Options.Applicative (Parser, ParserInfo, command, execParser, flag, fullDesc, help, helper,
                            info, infoFooter, infoHeader, long, metavar, optional, progDesc, short,
                            strArgument, subparser)
import Options.Applicative.Help.Chunk (stringChunk)

import Summoner.Ansi (boldText)
import Summoner.Default (endLine)
import Summoner.Project (Decision (..), Targets (..), generateProject)

import qualified Data.Text as T

----------------------------------------------------------------------------
-- CLI
----------------------------------------------------------------------------

summon :: IO ()
summon = execParser prsr >>= runWithOptions

-- | Run 'hs-init' with cli options
runWithOptions :: InitOpts -> IO ()
runWithOptions (InitOpts projectName targets) = do
     -- Generate the project.
    generateProject projectName targets

    boldText "\nJob's done"



-- | Initial parsed options from cli
data InitOpts = InitOpts Text Targets   -- ^ Includes the project name and target options.

targetsP ::  Decision -> Parser Targets
targetsP d = do
    githubFlag   <- githubP    d
    travisFlag   <- travisP    d
    appVeyorFlag <- appVeyorP  d
    privateFlag  <- privateP   d
    scriptFlag   <- scriptP    d
    isLibrary    <- libraryP   d
    isExecutable <- execP      d
    isTest       <- testP      d
    isBenchmark  <- benchmarkP d
    pure Targets{..}

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
testP d =  flag Idk d
        $  long "test"
        <> short 't'
        <> help "Test target"

benchmarkP :: Decision -> Parser Decision
benchmarkP d = flag Idk d
             $ long "benchmark"
            <> short 'b'
            <> help "Benchmarks"

onP :: Parser Targets
onP  = subparser $ mconcat
       [ metavar "on [OPTIONS]"
       , command "on" $ info (helper <*> targetsP Yes) (progDesc "Specify options to enable")
       ]

offP :: Parser Targets
offP = subparser $ mconcat
       [ metavar "off [OPTIONS]"
       , command "off" $ info (helper <*> targetsP Nop) (progDesc "Specify options to disable")
       ]

optsP :: Parser InitOpts
optsP = do
    projectName <- strArgument (metavar "PROJECT_NAME")
    on  <- optional onP
    off <- optional offP

    pure $ InitOpts projectName (fold $ on <> off)

prsr :: ParserInfo InitOpts
prsr = modifyHeader
     $ modifyFooter
     $ info ( helper <*> optsP )
            $ fullDesc
           <> progDesc "Create your own haskell project"

-- to put custom header which doesn't cut all spaces
modifyHeader :: ParserInfo InitOpts -> ParserInfo InitOpts
modifyHeader initOpts = initOpts {infoHeader = stringChunk $ T.unpack artHeader}

-- to put custom footer which doesn't cut all spaces
modifyFooter :: ParserInfo InitOpts -> ParserInfo InitOpts
modifyFooter initOpts = initOpts {infoFooter = stringChunk $ T.unpack artFooter}

artHeader :: Text
artHeader = [text|
$endLine
                                                   ___
                                                 /  .  \
                                                │\_/│   │
                                                │   │  /│
  ___________________________________________________-' │
 ╱                                                      │
╱   .-.                                                 │
│  /   \                                                │
│ |\_.  │ Summoner — tool for creating Haskell projects │
│\|  | /│                                               │
│ `-_-' │                                              ╱
│       │_____________________________________________╱
│       │
 ╲     ╱
  `-_-'
|]

artFooter :: Text
artFooter = [text|
$endLine
          ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
  ________┃                                  ┃_______
  ╲       ┃   λ Make Haskell Great Again λ   ┃      ╱
   ╲      ┃                                  ┃     ╱
   ╱      ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛     ╲
  ╱__________)                            (_________╲

|]
