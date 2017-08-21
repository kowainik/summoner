#!/usr/bin/env stack
-- stack runghc --package filepath --package directory --package aeson --package bytestring


{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Monad         (when)
import           Data.Aeson            (FromJSON (..), decodeStrict, withObject, (.:))
import           Data.ByteString.Char8 (pack)
import           Data.List             (intercalate, isInfixOf, isPrefixOf, nub)
import           Data.String           (IsString (..))
import           System.Directory      (doesPathExist, getCurrentDirectory,
                                        setCurrentDirectory)
import           System.Exit           (ExitCode (..))
import           System.FilePath       ((</>))
import           System.Process        (readProcess, showCommandForUser, system)
import           Text.Printf           (printf)

-----------------------
------ Settings -------
-----------------------

defaultOwner :: String
defaultOwner = "vrom911"

defaultName :: String
defaultName = "Veronika Romashkina"

defaultEmail :: String
defaultEmail = "vrom911@gmail.com"

defaultLicense :: String
defaultLicense = "MIT"

defaultGHC :: String
defaultGHC = "8.0.1"

defaultYear :: String
defaultYear = "2017"

--------------------------
--------- Script ---------
--------------------------

main :: IO ()
main = do
  owner <- queryDef "Repository owner: " defaultOwner
  repo  <- queryUniqueName
  description <- query "Short project description: "

  -- Generate the project.
  generateProject owner repo description

  -- Create the repository on Github.
  "git" ["init"]
  "hub" ["create", "-d", description, printf "%s/%s" owner repo]

 -- Make a commit and push it.
  "git" ["add", "."]
  "git" ["commit", "-m", "Create the project"]
  "git" ["push", "-u", "origin", "master"]

  putStrLn "Job's done"

-- | Generate the project.
generateProject :: String -> String -> String -> IO ()
generateProject owner repo description = do
  nm    <- queryDef "Author: " defaultName
  email    <- queryDef "Maintainer e-mail: " defaultEmail
  mapM_ putStrLn [
    "List of categories to choose from:",
    "",
    "  * Control                    * Concurrency",
    "  * Codec                      * Graphics",
    "  * Data                       * Sound",
    "  * Math                       * System",
    "  * Parsing                    * Network",
    "  * Text",
    "",
    "  * Application                * Development",
    "  * Compilers/Interpreters     * Testing",
    "  * Web",
    "  * Game",
    "  * Utility",
    ""]
  category <- query "Category: "
  license <- choose "License: "
    (nub (defaultLicense : licenseNames))

  -- License creation
  let licenseGithub = snd $ head $ dropWhile ((/= license) . fst) githubLicenseQueryNames
  let licenseLink  = "https://api.github.com/licenses/" ++ licenseGithub
  licenseJson <- readProcess "curl" [licenseLink, "-H", "Accept: application/vnd.github.drax-preview+json"] ""
  let licenseTxt =
       case (decodeStrict $ pack licenseJson) :: Maybe License of
           Just t  -> customizeLicense license (licenseText t) nm
           Nothing -> error "Broken predefined license list"

  -- Library or Executable flags
  (isLib, isExe) <- do
    ch <- choose "Library or Executable?" ["both", "lib", "exe"]
    case ch of
      "lib"  -> pure (True, False)
      "exe"  -> pure (False, True)
      "both" -> pure (True, True)

  putStrLn "Latest GHCs: 7.0.4 7.2.2 7.4.2 7.6.3 7.8.4 7.10.3 8.0.1"
  -- TODO: once GHC 7.8 is dropped, switch to <$>
  testedVersions <- words `fmap`
    queryDef "Versions of GHC to test with (space-separated): " defaultGHC
  let baseVer :: String
      baseVer
        | found "7.0"  = ">=4.3 && <5"
        | found "7.2"  = ">=4.4 && <5"
        | found "7.4"  = ">=4.5 && <5"
        | found "7.6"  = ">=4.6 && <5"
        | found "7.8"  = ">=4.7 && <5"
        | found "7.10" = ">=4.8 && <5"
        | found "8.0"  = ">=4.9 && <5"
        | otherwise    = "==4.*"
        where found v = any (\x -> (v++".") ~== x || v == x) testedVersions
  printf "Going to use this constraint on base: %s\n" baseVer
  longDescription <-
    queryDef "  Add longer description?" description
  -- create haskell template
  writeFile "temp.hsfiles" $ createStackTemplate repo owner longDescription license licenseTxt nm email category testedVersions baseVer isExe isLib
  -- create new project with stack
  "stack" ["new", repo, "temp.hsfiles"]
  -- do not need template file anymore
  "rm" ["temp.hsfiles"]
  "cd" [repo]

----------------------------------
---------- Utilities -------------
----------------------------------

licenseNames :: [String]
licenseNames = map fst githubLicenseQueryNames

githubLicenseQueryNames :: [(String, String)]
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

newtype License = License { licenseText :: String }

instance FromJSON License where
  parseJSON = withObject "License" $ \o -> License <$> o .: "body"

customizeLicense :: String -> String -> String -> String
customizeLicense l t nm
  | l `elem` words "MIT BSD2 BSD3" = updateLicenseText
  | otherwise = t
 where
  updateLicenseText =
    let (beforeY, withY) = span (/= '[') t
        afterY = tail $ dropWhile (/= ']') withY
        (beforeN, withN) = span (/= '[') afterY
        afterN = tail $ dropWhile (/= ']') withN in
    beforeY ++ defaultYear ++ beforeN ++ nm ++ afterN

-- TODO: once GHC 7.6 is dropped, just use callCommand
callCommand' :: String -> IO ()
callCommand' cmd = do
  exit_code <- system cmd
  case exit_code of
    ExitSuccess   -> return ()
    ExitFailure r -> error $ printf "%s failed with exit code %d" cmd r

-- This is needed to be able to call commands by writing strings.
instance (a ~ String, b ~ ()) => IsString ([a] -> IO b) where
  fromString "cd" [arg] = setCurrentDirectory arg
  fromString cmd args   = callCommand' (showCommandForUser cmd args)

printQuestion :: String -> [String] -> IO ()
printQuestion question (def:rest) =
  printf "%s [%s]%s " question def (concatMap ('/':) rest)
printQuestion question [] =
  printf "%s " question

choose :: String -> [String] -> IO String
choose question choices = do
  printQuestion question choices
  answer <- getLine
  if | null answer ->
         return (head choices)
     | answer `elem` choices ->
         return answer
     | otherwise -> do
         putStrLn "This wasn't a valid choice."
         choose question choices

query :: String -> IO String
query question = do
  printf "%s " question
  answer <- getLine
  if | null answer -> do
         putStrLn "An answer is required."
         query question
     | otherwise ->
         return answer

queryDef :: String -> String -> IO String
queryDef question defAnswer = do
  printf "%s [%s] " question defAnswer
  answer <- getLine
  if | null answer ->
         pure defAnswer
     | otherwise ->
         pure answer

queryUniqueName :: IO String
queryUniqueName = do
  repName <- query "Project name: "
  curPath <- getCurrentDirectory
  exist <- doesPathExist $ curPath </> repName
  if exist then do
    putStrLn "Project with this name is already exist. Please choose another one"
    queryUniqueName
  else
    pure repName

(~==), (=~=) :: String -> String -> Bool
(~==) = isPrefixOf
(=~=) = isInfixOf

splitOn :: String -> String -> [String]
splitOn _   ""  = []
splitOn sep str = go "" str
  where
    sepLen = length sep
    go acc s
      | null s && null acc = [""]
      | null s             = [reverse acc]
      | sep ~== s          = reverse acc : go "" (drop sepLen s)
      | otherwise          = go (head s : acc) (tail s)

replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old

-- | Creating template file to use in `stack new` command
createStackTemplate :: String  -- ^ repository name
                    -> String  -- ^ github username
                    -> String  -- ^ project description
                    -> String  -- ^ type of license
                    -> String  -- ^ license text
                    -> String  -- ^ full name
                    -> String  -- ^ e-mail
                    -> String  -- ^ project category
                    -> [String]  -- ^ ghc versions
                    -> String  -- ^ base version
                    -> Bool    -- ^ is executable
                    -> Bool    -- ^ is library
                    -> String  -- ^ template
createStackTemplate repo owner description license licenseText nm email cat testedVersions baseVer isExe isLib =
  createCabalTop ++
  (if isLib then createCabalLib  else "") ++
  (if isExe then createCabalExe  else "") ++
  (if isLib then createCabalTest else "") ++
  createCabalGit   ++
  createCabalFiles ++
  readme           ++
  gitignore        ++
  travisYml        ++
  changelog        ++
  createLicense

 where
  -- all basic project information for `*.cabal` file
  createCabalTop :: String
  createCabalTop = unlines [
    printf "{-# START_FILE %s.cabal #-}" repo,
    printf "name:                %s" repo,
    "version:             0.1.0.0",
    printf "description:             %s" (dropWhile (== ' ') description),
    printf "homepage:            https://github.com/%s/%s" owner repo,
    printf "bug-reports:         https://github.com/%s/%s/issues" owner repo,
    printf "license:             %s" license,
    "license-file:        LICENSE",
    printf "author:              %s" nm,
    printf "maintainer:          %s" email,
    printf "copyright:           %s %s" defaultYear nm,
    printf "category:            %s" cat,
    "build-type:          Simple",
    "extra-source-files:  README.md",
    "cabal-version:       >=1.10",
    testedWith,
    ""
    ]

  testedWith :: String
  testedWith = "tested-with:         " ++
          intercalate ", " (map ("GHC == " ++) testedVersions)

  createCabalLib :: String
  createCabalLib = unlines [
    "library",
    "  hs-source-dirs:      src",
    "  exposed-modules:     Lib",
    "  ghc-options:         -Wall",
    printf "  build-depends:       base %s" baseVer,
    "  default-language:    Haskell2010",
    ""
    ]

  createCabalExe :: String
  createCabalExe = unlines [
    printf "executable %s" repo,
    "  hs-source-dirs:      app",
    "  main-is:             Main.hs",
    "  ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N",
    "  build-depends:       base",
    printf "                     , %s" repo,
    "  default-language:    Haskell2010",
    ""
    ]

  createCabalTest :: String
  createCabalTest = unlines [
    printf "test-suite %s-test" repo,
    "  type:                exitcode-stdio-1.0",
    "  hs-source-dirs:      test",
    "  main-is:             Spec.hs",
    "  build-depends:       base",
    printf "                     , %s" repo,
    "  ghc-options:         -Wall -Werror -threaded -rtsopts -with-rtsopts=-N",
    "  default-language:    Haskell2010",
    ""
    ]

  createCabalGit :: String
  createCabalGit = unlines [
    "source-repository head",
    "  type:                git",
    printf "  location:            https://github.com/%s/%s.git" owner repo,
    ""
    ]

  createCabalFiles :: String
  createCabalFiles =
    createSetup ++
    (if isExe then createExe else "") ++
    (if isLib then createLib ++ createTest else "")

  createSetup :: String
  createSetup = unlines [
    "{-# START_FILE Setup.hs #-}",
    "import Distribution.Simple",
    "main = defaultMain",
    ""
    ]
  createTest :: String
  createTest = unlines [
    "{-# START_FILE test/Spec.hs #-}",
    "main :: IO ()",
    "main = putStrLn \"Test suite not yet implemented\"",
    ""
    ]

  createLib :: String
  createLib = unlines [
    "{-# START_FILE src/Lib.hs #-}",
    "module Lib",
    "    ( someFunc",
    "    ) where",
    "",
    "someFunc :: IO ()",
    "someFunc = putStrLn \"someFunc\"",
    ""
    ]

  createExe :: String
  createExe = unlines [
    "{-# START_FILE app/Main.hs #-}",
    "module Main where",
    "",
    "import Lib",
    "",
    "main :: IO ()",
    "main = someFunc",
    ""
    ]

  -- create README template
  readme :: String
  readme = unlines [
    "{-# START_FILE README.md #-}",
    printf "# %s" repo,
    "",
    printf "[![Hackage](%s)](%s)" hackageShield hackageLink,
    printf "[![Build status](%s)](%s)" travisShield travisLink,
    printf "[![%s license](%s)](%s)" license licenseShield licenseLink ]
    where
      hackageShield :: String =
        printf "https://img.shields.io/hackage/v/%s.svg" repo
      hackageLink :: String =
        printf "https://hackage.haskell.org/package/%s" repo
      travisShield :: String =
        printf "https://secure.travis-ci.org/%s/%s.svg" owner repo
      travisLink :: String =
        printf "https://travis-ci.org/%s/%s" owner repo
      licenseShield :: String =
        printf "https://img.shields.io/badge/license-%s-blue.svg" (replace "-" "--" license)
      licenseLink :: String =
        printf "https://github.com/%s/%s/blob/master/LICENSE" owner repo

  -- create .gitignore template
  gitignore :: String
  gitignore = unlines [
    "{-# START_FILE .gitignore #-}",
    "### Haskell",
    "dist",
    "dist-*",
    "cabal-dev",
    "*.o",
    "*.hi",
    "*.chi",
    "*.chs.h",
    "*.dyn_o",
    "*.dyn_hi",
    "*.prof",
    "*.aux",
    "*.hp",
    "*.eventlog",
    ".virtualenv",
    ".hsenv",
    ".hpc",
    ".cabal-sandbox/",
    "cabal.sandbox.config",
    "cabal.config",
    "cabal.project.local",
    ".HTF/",
    "# Stack",
    ".stack-work/",
    "",
    "### IDE/support",
    "# Vim",
    "[._]*.s[a-v][a-z]",
    "[._]*.sw[a-p]",
    "[._]s[a-v][a-z]",
    "[._]sw[a-p]",
    "*~",
    "tags",
    "",
    "# IntellijIDEA",
    ".idea/",
    ".ideaHaskellLib/",
    "*.iml",
    "",
    "# Atom",
    ".haskell-ghc-mod.json",
    "",
    "# VS",
    ".vscode/",
    "",
    "# Emacs",
    "*#",
    ".dir-locals.el",
    "TAGS",
    "",
    "# other",
    ".DS_Store",
    ""
    ]

  -- create CHANGELOG template
  changelog :: String
  changelog = unlines [
    "{-# START_FILE CHANGELOG.md #-}",
    "Change log",
    "==========",
    "",
    printf "%s uses [Semantic Versioning][1]." repo,
    "The change log is available [on GitHub][2].",
    "",
    "[1]: http://semver.org/spec/v2.0.0.html",
    printf "[2]: https://github.com/%s/%s/releases" owner repo,
    "",
    "# 0.1.0.0",
    "",
    "* Initially created."]

  createLicense :: String
  createLicense = "{-# START_FILE LICENSE #-}\n" ++ licenseText

  -- create travis.yml template
  travisYml :: String
  travisYml = unlines [
    "{-# START_FILE .travis.yml #-}",
    "# Use new container infrastructure to enable caching",
    "sudo: false",
    "",
    "# Choose a lightweight base image; we provide our own build tools.",
    "language: c",
    "",
    "# GHC depends on GMP. You can add other dependencies here as well.",
    "addons:",
    "  apt:",
    "    packages:",
    "    - libgmp-dev",
    "",
    "before_install:",
    "# Download and unpack the stack executable",
    "- mkdir -p ~/.local/bin",
    "- export PATH=$HOME/.local/bin:$PATH",
    "- travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'",
    "",
    "# This line does all of the work: installs GHC if necessary, builds the",
    "# library, executables, and test suites, and runs the test suites.",
    "# `--no-terminal works` around some quirks in Travis's terminal implementation.",
    "script: stack --no-terminal --install-ghc test",
    "",
    "# Caching so the next build will be fast too.",
    "cache:",
    "  directories:",
    "  - $HOME/.stack",
    printf "  - $HOME/build/%s/%s/.stack-work" owner repo
    ]
