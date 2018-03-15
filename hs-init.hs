#!/usr/bin/env stack
{- stack
  script
  --resolver lts-10.3
  --package aeson
  --package ansi-terminal
  --package bytestring
  --package directory
  --package filepath
  --package neat-interpolation
  --package optparse-applicative
  --package process
  --package text
  --package time
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

import Control.Exception
import Control.Monad (when)
import Data.Aeson (FromJSON (..), decodeStrict, withObject, (.:))
import Data.ByteString.Char8 (pack)
import Data.Foldable (fold)
import Data.List (nub)
import Data.Semigroup (Semigroup (..))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Time (getCurrentTime, toGregorian, utctDay)
import NeatInterpolation (text)
import Options.Applicative (Parser, ParserInfo, command, execParser, flag, fullDesc, help, helper,
                            info, infoFooter, infoHeader, long, metavar, optional, progDesc, short,
                            strArgument, subparser)
import Options.Applicative.Help.Chunk (stringChunk)
import System.Console.ANSI (Color (Blue, Green, Red, Yellow), ColorIntensity (Vivid),
                            ConsoleIntensity (BoldIntensity), ConsoleLayer (Foreground),
                            SGR (Reset, SetColor, SetConsoleIntensity), setSGR)
import System.Directory (doesPathExist, getCurrentDirectory, removeFile, setCurrentDirectory)
import System.FilePath ((</>))
import System.Info (os)
import System.IO (hFlush, hSetEncoding, stdout, utf8)
import System.Process (callProcess, readProcess)

import qualified Data.Text as T
import qualified Data.Text.IO as T

----------------------------------------------------------------------------
-- Default Settings
----------------------------------------------------------------------------

defaultOwner :: Text
defaultOwner = "vrom911"

defaultName :: Text
defaultName = "Veronika Romashkina"

defaultEmail :: Text
defaultEmail = "vrom911@gmail.com"

defaultLicense :: Text
defaultLicense = "MIT"

defaultGHC :: Text
defaultGHC = "8.2.2"

currentYear :: IO Text
currentYear = do
  now <- getCurrentTime
  let (year, _, _) = toGregorian $ utctDay now
  return $ T.pack $ show year

endLine :: Text
endLine = "\n"

----------------------------------------------------------------------------
-- Main Script
----------------------------------------------------------------------------

main :: IO ()
main = do
  hSetEncoding stdout utf8
  execParser prsr >>= runWithOptions

-- | Run 'hs-init' with cli options
runWithOptions :: InitOpts -> IO ()
runWithOptions (InitOpts projectName targets) = do
  repo        <- checkUniqueName projectName
  owner       <- queryDef "Repository owner: " defaultOwner
  description <- query "Short project description: "
  -- Generate the project.
  generateProject repo owner description targets

  bold
  T.putStrLn "\nJob's done"
  reset

-- | Generate the project.
generateProject :: Text -> Text -> Text -> Targets -> IO ()
generateProject repo owner description Targets{..} = do
  nm       <- queryDef "Author: " defaultName
  email    <- queryDef "Maintainer e-mail: " defaultEmail
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
  -- create stack project
  doStackCommands ProjectData{..}
  -- make b executable
  when script doScriptCommand
  -- create github repository and commit
  when github $ doGithubCommands privat

 where
  ifGithub :: Bool -> Text -> Decision -> IO Bool
  ifGithub github target decision = if github
    then decisionToBool decision target
    else falseMessage target

  doStackCommands :: ProjectData -> IO ()
  doStackCommands projectData = do
    -- create haskell template
    T.writeFile "temp.hsfiles" $ createStackTemplate projectData
    -- create new project with stack
    "stack" ["new", repo, "temp.hsfiles"]
    -- do not need template file anymore
    deleteFile "temp.hsfiles"
    "cd" [repo]

  doScriptCommand :: IO ()
  doScriptCommand = when (os /= "mingw32") ("chmod" ["+x", "b"])

  doGithubCommands :: Bool -> IO ()
  doGithubCommands private = do
    -- Create the repository on Github.
    "git" ["init"]
    "hub" $ ["create", "-d", description, owner <> "/" <> repo]
         ++ ["-p" | private] -- creates private repository if asked so.
    -- Make a commit and push it.
    "git" ["add", "."]
    "git" ["commit", "-m", "Create the project"]
    "git" ["push", "-u", "origin", "master"]

----------------------------------------------------------------------------
-- CLI
----------------------------------------------------------------------------

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


-- | Initial parsed options from cli
data InitOpts = InitOpts Text    -- ^ Project name
                         Targets -- ^ Target flags

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
githubP d =  flag Idk d
          $  long "github"
          <> short 'g'
          <> help "GitHub integration"

travisP :: Decision -> Parser Decision
travisP d =  flag Idk d
          $  long "travis"
          <> short 'c'
          <> help "Travis CI integration"

appVeyorP :: Decision -> Parser Decision
appVeyorP d =  flag Idk d
            $  long "app-veyor"
            <> short 'w'
            <> help "AppVeyor CI integration"

privateP :: Decision -> Parser Decision
privateP d =  flag Idk d
      $  long "private"
      <> short 'p'
      <> help "Private repository"

scriptP :: Decision -> Parser Decision
scriptP d = flag Idk d
          $ long "script"
         <> short 's'
         <> help "Build script for convenience"

libraryP :: Decision -> Parser Decision
libraryP d =  flag Idk d
           $  long "library"
           <> short 'l'
           <> help "Library folder"

execP :: Decision -> Parser Decision
execP d =  flag Idk d
        $  long "exec"
        <> short 'e'
        <> help "Executable target"

testP :: Decision -> Parser Decision
testP d =  flag Idk d
        $  long "test"
        <> short 't'
        <> help "Test target"

benchmarkP :: Decision -> Parser Decision
benchmarkP d =  flag Idk d
             $  long "benchmark"
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
    projectName <- T.pack <$> strArgument (metavar "PROJECT_NAME")
    on  <- optional onP
    off <- optional offP

    pure $ InitOpts projectName (fold $ on `mappend` off)

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
  __________________________________________________-' │
 ╱                                                     │
╱   .-.                                                │
│  /   \                                               │
│ |\_.  │ hs-init — tool for creating Haskell projects │
│\|  | /│                                              │
│ `-_-' │                                             ╱
│       │____________________________________________╱
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

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

data ProjectData = ProjectData
  { repo           :: Text   -- ^ repository name
  , owner          :: Text   -- ^ github username
  , description    :: Text   -- ^ project description
  , nm             :: Text   -- ^ full name
  , email          :: Text   -- ^ e-mail
  , year           :: Text   -- ^ year
  , category       :: Text   -- ^ project category
  , license        :: Text   -- ^ type of license
  , licenseText    :: Text   -- ^ license text
  , github         :: Bool   -- ^ github repository
  , travis         :: Bool   -- ^ Travis CI integration
  , appVey         :: Bool   -- ^ AppVeyor CI integration
  , script         :: Bool   -- ^ build script
  , isLib          :: Bool   -- ^ is library
  , isExe          :: Bool   -- ^ is executable
  , test           :: Bool   -- ^ add tests
  , bench          :: Bool   -- ^ add benchmarks
  , testedVersions :: [Text] -- ^ ghc versions
  } deriving Show

decisionToBool :: Decision -> Text -> IO Bool
decisionToBool decision target =
  case decision of
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

----------------------------------------------------------------------------
-- Ansi-terminal
----------------------------------------------------------------------------

-- Explicit flush ensures prompt messages are in the correct order on all systems.
putStrFlush :: Text -> IO ()
putStrFlush msg = do
  T.putStr msg
  hFlush stdout

setColor :: Color -> IO ()
setColor color = setSGR [SetColor Foreground Vivid color]

bold :: IO ()
bold = setSGR [SetConsoleIntensity BoldIntensity]

reset :: IO ()
reset = do
  setSGR [Reset]
  hFlush stdout

prompt :: IO Text
prompt = do
  setColor Blue
  putStrFlush "  ->   "
  reset
  T.getLine

boldDefault :: Text -> IO ()
boldDefault message = do
  bold
  putStrFlush (" [" <> message <> "]")
  reset

colorMessage :: Color -> Text -> IO ()
colorMessage color message = do
  setColor color
  T.putStrLn $ "  " <> message
  reset

errorMessage, warningMessage, successMessage :: Text -> IO ()
errorMessage   = colorMessage Red
warningMessage = colorMessage Yellow
successMessage = colorMessage Green

----------------------------------------------------------------------------
-- License
----------------------------------------------------------------------------

licenseNames :: [Text]
licenseNames = map fst githubLicenseQueryNames

githubLicenseQueryNames :: [(Text, Text)]
githubLicenseQueryNames =
  [ ("MIT",        "mit")
  , ("BSD2",       "bsd-2-clause")
  , ("BSD3",       "bsd-3-clause")
  , ("GPL-2",      "gpl-2.0")
  , ("GPL-3",      "gpl-3.0")
  , ("LGPL-2.1",   "lgpl-2.1")
  , ("LGPL-3",     "lgpl-3.0")
  , ("AGPL-3",     "agpl-3.0")
  , ("Apache-2.0", "apache-2.0")
  , ("MPL-2.0",    "mpl-2.0")
  ]

newtype License = License { lcnsText :: Text }

instance FromJSON License where
  parseJSON = withObject "License" $ \o -> License <$> o .: "body"

customizeLicense :: Text -> Text -> Text -> Text -> Text
customizeLicense l t nm year
  | l `elem` T.words "MIT BSD2 BSD3" = updateLicenseText
  | otherwise = t
 where
  updateLicenseText =
    let (beforeY, withY) = T.span (/= '[') t
        afterY = T.tail $ T.dropWhile (/= ']') withY
        (beforeN, withN) = T.span (/= '[') afterY
        afterN = T.tail $ T.dropWhile (/= ']') withN in
    beforeY <> year <> beforeN <> nm <> afterN

----------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------

-- This is needed to be able to call commands by writing strings.
instance (a ~ Text, b ~ ()) => IsString ([a] -> IO b) where
  fromString "cd" [arg] = setCurrentDirectory $ T.unpack arg
  fromString cmd args   = callProcess cmd (map T.unpack args)

-- Delete file, but just print a message if delete fails and continue instead of raising an error.
deleteFile :: FilePath -> IO  ()
deleteFile file = catch (removeFile file) printError
  where printError (e :: SomeException) = errorMessage $ "Could not delete file '" <> T.pack file <> "'. " <> T.pack  (displayException e)

----------------------------------------------------------------------------
-- IO Questioning
----------------------------------------------------------------------------

printQuestion :: Text -> [Text] -> IO ()
printQuestion question (def:rest) = do
  let restSlash = T.intercalate "/" rest
  putStrFlush question
  boldDefault def
  T.putStrLn $ "/" <> restSlash
printQuestion question [] =
  T.putStrLn question

choose :: Text -> [Text] -> IO Text
choose question choices = do
  printQuestion question choices
  answer <- prompt
  if | T.null answer ->
         return (head choices)
     | answer `elem` choices ->
         return answer
     | otherwise -> do
         errorMessage "This wasn't a valid choice."
         choose question choices

query :: Text -> IO Text
query question = do
  T.putStrLn question
  answer <- prompt
  if | T.null answer -> do
         errorMessage "An answer is required."
         query question
     | otherwise ->
         return answer

queryDef :: Text -> Text -> IO Text
queryDef question defAnswer = do
  putStrFlush question
  boldDefault defAnswer
  T.putStrLn ""
  answer <- prompt
  if | T.null answer ->
         pure defAnswer
     | otherwise ->
         pure answer

checkUniqueName :: Text -> IO Text
checkUniqueName nm = do
  curPath <- getCurrentDirectory
  exist   <- doesPathExist $ curPath </> T.unpack nm
  if exist then do
    errorMessage "Project with this name is already exist. Please choose another one"
    newNm <- query "Project name: "
    checkUniqueName newNm
  else
    pure nm

----------------------------------------------------------------------------
-- Stack File Creation
----------------------------------------------------------------------------

emptyIfNot :: Bool -> Text -> Text
emptyIfNot p txt = if p then txt else ""

-- | Creating template file to use in `stack new` command
createStackTemplate :: ProjectData ->  Text
createStackTemplate
  ProjectData{..} = createCabalTop
                 <> emptyIfNot isLib createCabalLib
                 <> emptyIfNot isExe
                               ( createCabalExe
                               $ emptyIfNot isLib $ ", " <> repo )
                 <> emptyIfNot test createCabalTest
                 <> emptyIfNot bench
                               ( createCabalBenchmark
                               $ emptyIfNot isLib $ ", " <> repo )
                 <> emptyIfNot github createCabalGit
                 <> createCabalFiles
                 <> readme
                 <> emptyIfNot github gitignore
                 <> emptyIfNot travis travisYml
                 <> emptyIfNot appVey appVeyorYml
                 <> emptyIfNot script scriptSh
                 <> changelog
                 <> createLicense
                 <> createStackYamls testedVersions
 where
  -- all basic project information for `*.cabal` file
  createCabalTop :: Text
  createCabalTop =
    [text|
    {-# START_FILE ${repo}.cabal #-}
    name:                $repo
    version:             0.0.0
    description:         $description
    synopsis:            $description
    homepage:            https://github.com/${owner}/${repo}
    bug-reports:         https://github.com/${owner}/${repo}/issues
    license:             $license
    license-file:        LICENSE
    author:              $nm
    maintainer:          $email
    copyright:           $year $nm
    category:            $category
    build-type:          Simple
    extra-doc-files:     README.md
    cabal-version:       >=1.24
    $testedWith
    $endLine
    |]

  testedWith :: Text
  testedWith = "tested-with:         GHC == " <> defaultGHC <>
          T.concat (map (", GHC == " <>) testedVersions)

  createCabalLib :: Text
  createCabalLib =
    [text|
    library
      hs-source-dirs:      src
      exposed-modules:     Lib
      ghc-options:         -Wall
      build-depends:       base
      default-language:    Haskell2010
    $endLine
    |]

  createCabalExe :: Text -> Text
  createCabalExe r =
    [text|
    executable $repo
      hs-source-dirs:      app
      main-is:             Main.hs
      ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N
      build-depends:       base
                         $r
      default-language:    Haskell2010
    $endLine
    |]

  createCabalTest :: Text
  createCabalTest =
    [text|
    test-suite ${repo}-test
      type:                exitcode-stdio-1.0
      hs-source-dirs:      test
      main-is:             Spec.hs
      build-depends:       base
                         , $repo
      ghc-options:         -Wall -Werror -threaded -rtsopts -with-rtsopts=-N
      default-language:    Haskell2010
    $endLine
    |]

  createCabalBenchmark :: Text -> Text
  createCabalBenchmark r =
    [text|
    benchmark ${repo}-benchmark
      type:                exitcode-stdio-1.0
      default-language:    Haskell2010
      ghc-options:         -Wall -Werror -O2 -threaded -rtsopts -with-rtsopts=-N
      hs-source-dirs:      benchmark
      main-is:             Main.hs
      build-depends:       base
                         , criterion
                         $r
    $endLine
    |]

  createCabalGit :: Text
  createCabalGit =
    [text|
    source-repository head
      type:                git
      location:            https://github.com/${owner}/${repo}.git
    $endLine
    |]

  createCabalFiles :: Text
  createCabalFiles =
       emptyIfNot isExe (if isLib then createExe else createOnlyExe)
    <> emptyIfNot isLib createLib
    <> emptyIfNot test  createTest
    <> emptyIfNot bench createBenchmark

  createTest :: Text
  createTest =
    [text|
    {-# START_FILE test/Spec.hs #-}
    main :: IO ()
    main = putStrLn "Test suite not yet implemented"
    $endLine
    |]

  createLib :: Text
  createLib =
    [text|
    {-# START_FILE src/Lib.hs #-}
    module Lib
        ( someFunc
        ) where

    someFunc :: IO ()
    someFunc = putStrLn "someFunc"
    $endLine
    |]

  createOnlyExe :: Text
  createOnlyExe =
    [text|
    {-# START_FILE app/Main.hs #-}
    module Main where

    main :: IO ()
    main = putStrLn "Hello, world!"
    $endLine
    |]

  createExe :: Text
  createExe =
    [text|
    {-# START_FILE app/Main.hs #-}
    module Main where

    import Lib

    main :: IO ()
    main = someFunc
    $endLine
    |]

  createBenchmark :: Text
  createBenchmark =
    [text|
    {-# START_FILE benchmark/Main.hs #-}
    import Criterion.Main

    main :: IO ()
    main = defaultMain [bench "const" (whnf const ())]
    $endLine
    |]

  -- create README template
  readme :: Text
  readme =
    [text|
    {-# START_FILE README.md #-}
    # $repo

    [![Hackage]($hackageShield)]($hackageLink)
    [![Build status](${travisShield})](${travisLink})
    [![Windows build status](${appVeyorShield})](${appVeyorLink})
    [![$license license](${licenseShield})](${licenseLink})

    $description
    $endLine
    |]
    where
      hackageShield :: Text =
        "https://img.shields.io/hackage/v/" <> repo <> ".svg"
      hackageLink :: Text =
        "https://hackage.haskell.org/package/" <> repo
      travisShield :: Text =
        "https://secure.travis-ci.org/" <> owner <> "/" <> repo <> ".svg"
      travisLink :: Text =
        "https://travis-ci.org/" <> owner <> "/" <> repo
      appVeyorShield :: Text =
        "https://ci.appveyor.com/api/projects/status/github/" <> owner <> "/" <> repo <> "?branch=master&svg=true"
      appVeyorLink :: Text =
        "https://ci.appveyor.com/project/" <> owner <> "/" <> repo
      licenseShield :: Text =
        "https://img.shields.io/badge/license-" <> T.replace "-" "--" license <> "-blue.svg"
      licenseLink :: Text =
        "https://github.com/" <> owner <> "/" <> repo <> "/blob/master/LICENSE"

  -- create .gitignore template
  gitignore :: Text
  gitignore =
    [text|
    {-# START_FILE .gitignore #-}
    ### Haskell
    dist
    dist-*
    cabal-dev
    *.o
    *.hi
    *.chi
    *.chs.h
    *.dyn_o
    *.dyn_hi
    *.prof
    *.aux
    *.hp
    *.eventlog
    .virtualenv
    .hsenv
    .hpc
    .cabal-sandbox/
    cabal.sandbox.config
    cabal.config
    cabal.project.local
    .HTF/
    # Stack
    .stack-work/

    ### IDE/support
    # Vim
    [._]*.s[a-v][a-z]
    [._]*.sw[a-p]
    [._]s[a-v][a-z]
    [._]sw[a-p]
    *~
    tags

    # IntellijIDEA
    .idea/
    .ideaHaskellLib/
    *.iml

    # Atom
    .haskell-ghc-mod.json

    # VS
    .vscode/

    # Emacs
    *#
    .dir-locals.el
    TAGS

    # other
    .DS_Store
    $endLine
    |]

  -- create CHANGELOG template
  changelog :: Text
  changelog =
    [text|
    {-# START_FILE CHANGELOG.md #-}
    Change log
    ==========

    $repo uses [PVP Versioning][1].
    The change log is available [on GitHub][2].

    0.0.0
    =====
    * Initially created.

    [1]: https://pvp.haskell.org
    [2]: https://github.com/${owner}/${repo}/releases
    $endLine
    |]

  createLicense :: Text
  createLicense = "{-# START_FILE LICENSE #-}\n" <> licenseText

  -- create travis.yml template
  travisYml :: Text
  travisYml =
    let travisMtr = T.concat (map travisMatrixItem testedVersions) in
    [text|
    {-# START_FILE .travis.yml #-}
    sudo: true
    language: haskell

    git:
      depth: 5

    cache:
      directories:
      - "$$HOME/.stack"
      - "$$HOME/build/${owner}/${repo}/.stack-work"

    matrix:
      include:

      $travisMtr

      - ghc: $defaultGHC
        env: GHCVER='${defaultGHC}' STACK_YAML="$$HOME/build/${owner}/${repo}/stack.yaml"

    addons:
      apt:
        sources:
          - sourceline: 'ppa:hvr/ghc'
        packages:
          - libgmp-dev

    before_install:
      - mkdir -p ~/.local/bin
      - export PATH="$$HOME/.local/bin:$$PATH"
      - travis_retry curl -L 'https://www.stackage.org/stack/linux-x86_64' | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
      - stack --version


    install:
      - travis_wait 30 stack setup --no-terminal
      - stack ghc -- --version

      - travis_wait 40 stack build --only-dependencies --no-terminal
      - travis_wait 40 stack build --test --bench --haddock --no-run-tests --no-run-benchmarks --no-haddock-deps --no-terminal

    script:
      - travis_wait 40 stack build --test --no-terminal

    notifications:
      email: false
    $endLine
    |]

  travisMatrixItem :: Text -> Text
  travisMatrixItem ghcV =
    [text|
    - ghc: ${ghcV}
      env: GHCVER='${ghcV}' STACK_YAML="$$HOME/build/${owner}/${repo}/stack-$$GHCVER.yaml"
    $endLine
    |]

  -- create specified @stack.yaml@ files
  createStackYamls :: [Text] -> Text
  createStackYamls = T.concat . map createStackYaml
   where
    createStackYaml :: Text -> Text
    createStackYaml ghc = case ghc of
        "8.0.2"  -> stackYaml "9.21"
        "7.10.3" -> stackYaml "6.35"
        _        -> ""
      where
        stackYaml :: Text -> Text
        stackYaml lts =
            [text|
            {-# START_FILE stack-${ghc}.yaml #-}
            resolver: lts-${lts}

            $endLine
            |]

  -- create appveyor.yml template
  appVeyorYml :: Text
  appVeyorYml =
    [text|
    {-# START_FILE appveyor.yml #-}
    build: off

    before_test:
    # http://help.appveyor.com/discussions/problems/6312-curl-command-not-found
    - set PATH=C:\Program Files\Git\mingw64\bin;%PATH%

    - curl -sS -ostack.zip -L --insecure http://www.stackage.org/stack/windows-i386
    - 7z x stack.zip stack.exe

    clone_folder: "c:\\stack"
    environment:
      global:
        STACK_ROOT: "c:\\sr"

    test_script:
    - stack setup > nul
    # The ugly echo "" hack is to avoid complaints about 0 being an invalid file
    # descriptor
    - echo "" | stack --no-terminal build --bench --no-run-benchmarks --test
    |]

  scriptSh :: Text
  scriptSh =
    [text|
    {-# START_FILE b #-}
    #!/usr/bin/env bash
    set -e

    # DESCRIPTION
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # This script builds the project in a way that is convenient for developers.
    # It passes the right flags into right places, builds the project with --fast,
    # tidies up and highlights error messages in GHC output.

    # USAGE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #   ./b                 build whole project with all targets
    #   ./b -c              do stack clean
    #   ./b -t              build and run tests
    #   ./b -b              build and run benchmarks
    #   ./b --nix           use nix to build package

    args=''
    test=false
    bench=false
    with_nix=false
    clean=false

    for var in "$$@"
    do
      # -t = run tests
      if [[ $$var == "-t" ]]; then
        test=true
      # -b = run benchmarks
      elif [[ $$var == "-b" ]]; then
        bench=true
      elif [[ $$var == "--nix" ]]; then
        with_nix=true
      # -c = clean
      elif [[ $$var == "-c" ]]; then
        clean=true
      else
        args="$$args $$var"
      fi
    done

    # Cleaning project
    if [[ $$clean == true ]]; then
      echo "Cleaning project..."
      stack clean
      exit
    fi

    if [[ $$no_nix == true ]]; then
      args="$$args --nix"
    fi

    xperl='$|++; s/(.*) Compiling\s([^\s]+)\s+\(\s+([^\/]+).*/\1 \2/p'
    xgrep="((^.*warning.*$|^.*error.*$|^    .*$|^.*can't find source.*$|^Module imports form a cycle.*$|^  which imports.*$)|^)"

    stack build $$args                                    \
                --ghc-options="+RTS -A256m -n2m -RTS"    \
                --test                                   \
                --no-run-tests                           \
                --no-haddock-deps                        \
                --bench                                  \
                --no-run-benchmarks                      \
                --jobs=4                                 \
                --dependencies-only

    stack build $$args                                    \
                --fast                                   \
                --ghc-options="+RTS -A256m -n2m -RTS"    \
                --test                                   \
                --no-run-tests                           \
                --no-haddock-deps                        \
                --bench                                  \
                --no-run-benchmarks                      \
                --jobs=4 2>&1 | perl -pe "$$xperl" | { grep -E --color "$$xgrep" || true; }

    if [[ $$test == true ]]; then
      stack build $$args                                  \
                  --fast                                 \
                  --ghc-options="+RTS -A256m -n2m -RTS"  \
                  --test                                 \
                  --no-haddock-deps                      \
                  --bench                                \
                  --no-run-benchmarks                    \
                  --jobs=4
    fi

    if [[ $$bench == true ]]; then
      stack build $$args                                  \
                  --fast                                 \
                  --ghc-options="+RTS -A256m -n2m -RTS"  \
                  --test                                 \
                  --no-run-tests                         \
                  --no-haddock-deps                      \
                  --bench                                \
                  --jobs=4
    fi
    $endLine
    |]
