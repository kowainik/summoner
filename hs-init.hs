#!/usr/bin/env stack
{- stack
  script
  --resolver lts-8.21
  --package filepath
  --package directory
  --package aeson
  --package bytestring
  --package process
  --package optparse-applicative
  --package text
  --package neat-interpolation
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Monad         (when)
import           Data.Aeson            (FromJSON (..), decodeStrict, withObject, (.:))
import           Data.ByteString.Char8 (pack)
import           Data.List             (nub)
import           Data.Semigroup        ((<>))
import           Data.String           (IsString (..))
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           NeatInterpolation     (text)
import           Options.Applicative   (Parser (), ParserInfo (), execParser, footer,
                                        fullDesc, header, help, helper, info, long,
                                        metavar, progDesc, short, strArgument, switch)
import           System.Directory      (doesPathExist, getCurrentDirectory,
                                        setCurrentDirectory)
import           System.FilePath       ((</>))
import           System.Process        (callCommand, readProcess, showCommandForUser)

-----------------------
------ Settings -------
-----------------------

defaultOwner :: Text
defaultOwner = "vrom911"

defaultName :: Text
defaultName = "Veronika Romashkina"

defaultEmail :: Text
defaultEmail = "vrom911@gmail.com"

defaultLicense :: Text
defaultLicense = "MIT"

defaultGHC :: Text
defaultGHC = "8.0.1"

defaultYear :: Text
defaultYear = "2017"

--------------------------
--------- Script ---------
--------------------------

main :: IO ()
main = execParser prsr >>= runWithOptions

runWithOptions :: InitOpts -> IO ()
runWithOptions opts@InitOpts{..} = do
  repo        <- checkUniqueName projectName
  owner       <- queryDef "Repository owner: " defaultOwner
  description <- query "Short project description: "

  -- Generate the project.
  generateProject repo owner description opts
  when githubFlag $ do
    -- Create the repository on Github.
    "git" ["init"]
    "hub" ["create", "-d", description, owner <> "/" <> repo]

    -- Make a commit and push it.
    "git" ["add", "."]
    "git" ["commit", "-m", "Create the project"]
    "git" ["push", "-u", "origin", "master"]

  putStrLn "Job's done"

---------------------------
---------- CLI ------------
---------------------------

data InitOpts = InitOpts
  { projectName  :: Text
  , githubFlag   :: Bool
--  , ciFlag      :: Bool
  , isLibrary    :: Bool
  , isExecutable :: Bool
  , isTest       :: Bool
  , isBenchmark  :: Bool
  }

githubP :: Parser Bool
githubP = switch
      (  long "github"
      <> short 'g'
      <> help "Enable GitHub integration"
      )

libraryP :: Parser Bool
libraryP = switch
      (  long "library"
      <> short 'l'
      <> help "Create library folder"
      )

execP :: Parser Bool
execP = switch
      (  long "exec"
      <> short 'e'
      <> help "Create executable target"
      )

testP :: Parser Bool
testP = switch
      (  long "test"
      <> short 't'
      <> help "Create test target"
      )

benchmarkP :: Parser Bool
benchmarkP = switch
      (  long "benchmark"
      <> short 'b'
      <> help "Create benchmarks"
      )

optsP :: Parser InitOpts
optsP = do
    projectName  <- T.pack <$> strArgument (metavar "PROJECT_NAME")
    githubFlag   <- githubP
    isLibrary    <- libraryP
    isExecutable <- execP
    isTest       <- testP
    isBenchmark  <- benchmarkP
    pure InitOpts{..}

prsr :: ParserInfo InitOpts
prsr = info ( helper <*> optsP )
            ( fullDesc
           <> progDesc "Create your own haskell project"
           <> header "hs-init -- tool for creating completely configured production Haskell projects"
           <> footer "hs-init test footer" )
---------------------------

-- | Generate the project.
generateProject :: Text -> Text -> Text -> InitOpts -> IO ()
generateProject repo owner description InitOpts{..} = do
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
  license  <- choose "License: "
    (nub (defaultLicense : licenseNames))

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
  let licenseText = case (decodeStrict $ pack licenseJson) :: Maybe License of
          Just t  -> customizeLicense license (lcnsText t) nm
          Nothing -> error "Broken predefined license list"

  -- Library or Executable flags
  (isLib, isExe) <-
    if isLibrary then
        pure (True, isExecutable)
    else if isExecutable then
        pure (False, True)
    else do
        ch <- choose "Library or Executable?" ["both", "lib", "exe"]
        pure $ case ch of
          "lib"  -> (True, False)
          "exe"  -> (False, True)
          "both" -> (True, True)
  test <-
    if isTest then pure True
    else do
      ch <- choose "Add tests?" ["y", "n"]
      case ch of
        "y" -> True <$ T.putStrLn "Tests will be added to the project"
        "n" -> pure False
  bench <-
    if isBenchmark then pure True
    else do
      ch <- choose "Add benchmarks?" ["y", "n"]
      case ch of
        "y" -> True <$ T.putStrLn "Benchmarks will be added to the project"
        "n" -> pure False

  putStrLn "Latest GHCs: 7.0.4 7.2.2 7.4.2 7.6.3 7.8.4 7.10.3 8.0.1 8.2.1"
  -- TODO: once GHC 7.8 is dropped, switch to <$>
  testedVersions <- T.words `fmap`
    queryDef "Versions of GHC to test with (space-separated): " defaultGHC
  -- create haskell template

  T.writeFile "temp.hsfiles"
            $ createStackTemplate ProjectData{..}

  -- create new project with stack
  "stack" ["new", repo, "temp.hsfiles"]
  -- do not need template file anymore
  "rm" ["temp.hsfiles"]
  "cd" [repo]

----------------------------------
---------- Utilities -------------
----------------------------------

data ProjectData = ProjectData
  { repo           :: Text   -- ^ repository name
  , owner          :: Text   -- ^ github username
  , description    :: Text   -- ^ project description
  , nm             :: Text   -- ^ full name
  , email          :: Text   -- ^ e-mail
  , category       :: Text   -- ^ project category
  , license        :: Text   -- ^ type of license
  , licenseText    :: Text   -- ^ license text
  , isLib          :: Bool   -- ^ is library
  , isExe          :: Bool   -- ^ is executable
  , test           :: Bool   -- ^ add tests
  , bench          :: Bool   -- ^ add benchmarks
  , testedVersions :: [Text] -- ^ ghc versions
  }
  deriving Show

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

customizeLicense :: Text -> Text -> Text -> Text
customizeLicense l t nm
  | l `elem` T.words "MIT BSD2 BSD3" = updateLicenseText
  | otherwise = t
 where
  updateLicenseText =
    let (beforeY, withY) = T.span (/= '[') t
        afterY = T.tail $ T.dropWhile (/= ']') withY
        (beforeN, withN) = T.span (/= '[') afterY
        afterN = T.tail $ T.dropWhile (/= ']') withN in
    beforeY <> defaultYear <> beforeN <> nm <> afterN

-- This is needed to be able to call commands by writing strings.
instance (a ~ Text, b ~ ()) => IsString ([a] -> IO b) where
  fromString "cd" [arg] = setCurrentDirectory $ T.unpack arg
  fromString cmd args   = callCommand $ showCommandForUser cmd (map T.unpack args)

printQuestion :: Text -> [Text] -> IO ()
printQuestion question (def:rest) = do
  let restSlash = T.intercalate "/" rest
  T.putStr $ question <> " [" <> def<> "]/" <> restSlash <> "  "
printQuestion question [] =
  T.putStr $ question <> "  "

choose :: Text -> [Text] -> IO Text
choose question choices = do
  printQuestion question choices
  answer <- T.getLine
  if | T.null answer ->
         return (head choices)
     | answer `elem` choices ->
         return answer
     | otherwise -> do
         T.putStrLn "This wasn't a valid choice."
         choose question choices

query :: Text -> IO Text
query question = do
  T.putStr $ question <> "  "
  answer <- T.getLine
  if | T.null answer -> do
         T.putStrLn "An answer is required."
         query question
     | otherwise ->
         return answer

queryDef :: Text -> Text -> IO Text
queryDef question defAnswer = do
  T.putStr $ question <> " [" <> defAnswer <> "]  "
  answer <- T.getLine
  if | T.null answer ->
         pure defAnswer
     | otherwise ->
         pure answer

checkUniqueName :: Text -> IO Text
checkUniqueName nm = do
  curPath <- getCurrentDirectory
  exist   <- doesPathExist $ curPath </> T.unpack nm
  if exist then do
    T.putStrLn "Project with this name is already exist. Please choose another one"
    newNm <- query "Project name: "
    checkUniqueName newNm
  else
    pure nm

-- | Creating template file to use in `stack new` command
createStackTemplate :: ProjectData -> Text
createStackTemplate
  ProjectData{..} = createCabalTop
                 <> (if isLib
                     then createCabalLib
                     else "")
                 <> (if isExe
                     then createCabalExe (if isLib
                                          then ", " <> repo
                                          else "")
                     else "")
                 <> (if test
                    then createCabalTest
                    else "")
                 <> (if bench
                     then createCabalBenchmark (if isLib
                                                then ", " <> repo
                                                else "")
                     else "")
                 <> createCabalGit
                 <> createCabalFiles
                 <> readme
                 <> gitignore
                 <> travisYml
                 <> changelog
                 <> createLicense

 where
  -- all basic project information for `*.cabal` file
  createCabalTop :: Text
  createCabalTop =
    [text|
    {-# START_FILE ${repo}.cabal #-}
    name:                $repo
    version:             0.1.0.0
    description:         $description
    homepage:            https://github.com/${owner}/${repo}
    bug-reports:         https://github.com/${owner}/${repo}/issues
    license:             $license
    license-file:        LICENSE
    author:              $nm
    maintainer:          $email
    copyright:           $defaultYear $nm
    category:            $category
    build-type:          Simple
    extra-source-files:  README.md
    cabal-version:       >=1.10
    $testedWith

    |]

  testedWith :: Text
  testedWith = "tested-with:         " <>
          T.intercalate ", " (map ("GHC == " <>) testedVersions)

  createCabalLib :: Text
  createCabalLib =
    [text|
    library
      hs-source-dirs:      src
      exposed-modules:     Lib
      ghc-options:         -Wall
      build-depends:       base
      default-language:    Haskell2010

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

    |]

  createCabalGit :: Text
  createCabalGit =
    [text|
    source-repository head
      type:                git
      location:            https://github.com/${owner}/${repo}.git

    |]

  createCabalFiles :: Text
  createCabalFiles =
       createSetup
    <> (if isExe then if isLib then createExe else createOnlyExe else  "")
    <> (if isLib then createLib else  "")
    <> (if test  then createTest else "")
    <> (if bench then createBenchmark else "")

  createSetup :: Text
  createSetup =
    [text|
    {-# START_FILE Setup.hs #-}
    import Distribution.Simple

    main = defaultMain
    |]

  createTest :: Text
  createTest =
    [text|
    {-# START_FILE test/Spec.hs #-}
    main :: IO ()
    main = putStrLn "Test suite not yet implemented"
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

    |]

  createOnlyExe :: Text
  createOnlyExe =
    [text|
    {-# START_FILE app/Main.hs #-}
    module Main where

    main :: IO ()
    main = putStrLn "Hello, world!"

    |]

  createExe :: Text
  createExe =
    [text|
    {-# START_FILE app/Main.hs #-}
    module Main where

    import Lib

    main :: IO ()
    main = someFunc

    |]

  createBenchmark :: Text
  createBenchmark =
    [text|
    {-# START_FILE benchmark/Main.hs #-}
    import Criterion.Main

    main :: IO ()
    main = defaultMain [bench "const" (whnf const ())]

    |]

  -- create README template
  readme :: Text
  readme =
    [text|
    {-# START_FILE README.md #-}
    # $repo

    [![Hackage]($hackageShield)]($hackageLink)
    [![Build status](${travisShield})](${travisLink})
    [![$license license](${licenseShield})](${licenseLink})
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

    [1]: https://pvp.haskell.org
    [2]: https://github.com/${owner}/${repo}/releases
    # 0.1.0.0
    * Initially created.
    |]

  createLicense :: Text
  createLicense = "{-# START_FILE LICENSE #-}\n" <> licenseText

  -- create travis.yml template
  travisYml :: Text
  travisYml =
    [text|
    {-# START_FILE .travis.yml #-}
    # Use new container infrastructure to enable caching
    sudo: false

    # Choose a lightweight base image; we provide our own build tools.
    language: c

    # GHC depends on GMP. You can add other dependencies here as well.
    addons:
      apt:
        packages:
        - libgmp-dev

    before_install:
    # Download and unpack the stack executable
    - mkdir -p ~/.local/bin
    - export PATH=$$HOME/.local/bin:$$PATH
    - travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

    # This line does all of the work: installs GHC if necessary, builds the
    # library, executables, and test suites, and runs the test suites.
    # `--no-terminal works` around some quirks in Travis's terminal implementation.
    script: stack --no-terminal --install-ghc test

    # Caching so the next build will be fast too.
    cache:
      directories:
      - $$HOME/.stack
      - $$HOME/build/$owner/${repo}/.stack-work
    |]
