#!/usr/bin/env stack
-- stack runghc --package filepath --package directory


{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Monad    (when)
import           Data.List        (intercalate, isInfixOf, isPrefixOf, nub)
import           Data.String      (IsString (..))
import           System.Directory (doesPathExist, getCurrentDirectory,
                                   setCurrentDirectory)
import           System.Exit      (ExitCode (..))
import           System.FilePath  ((</>))
import           System.Process   (showCommandForUser, system)
import           Text.Printf      (printf)

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
    "Here are some categories:",
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
  licenseCabal <- choose "License: "
    (nub (defaultLicense : words "BSD3 BSD2 GPL-3 GPL-2 MIT PublicDomain"))
  when (licenseCabal == "PublicDomain") $
    putStrLn "generating a LICENSE file with CC0."

  let license = if licenseCabal == "PublicDomain" then "CC0" else licenseCabal

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
  writeFile "temp.hsfiles" $ createStackTemplate repo owner longDescription license nm email category testedVersions baseVer
  -- create new project with stack
  "stack" ["new", repo, "temp.hsfiles"]
  -- do not need template file anymore
  "rm" ["temp.hsfiles"]
  "cd" [repo]

----------------------------------
---------- Utilities -------------
----------------------------------

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

(==>) :: a -> b -> (a, b)
(==>) = (,)

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
                    -> String  -- ^ full name
                    -> String  -- ^ e-mail
                    -> String  -- ^ project category
                    -> [String]  -- ^ ghc versions
                    -> String  -- ^ base version
                    -> String  -- ^ template
createStackTemplate repo owner description license nm email cat testedVersions baseVer =
  createCabalTop   ++
  createCabalLib   ++
  createCabalExe   ++
  createCabalTest  ++
  createCabalGit   ++
  createCabalFiles ++
  readme           ++
  gitignore        ++
  travisYml        ++
  changelog        ++
  case license of
    "MIT"  -> mitLicense
    "BSD2" -> bsd2License
    "BSD3" -> bsd3License
    _      -> publicDomainLicense

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
    printf "copyright:           2017 %s" nm,
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
    "  ghc-options:         -Wall -Werror -threaded -rtsopts -with-rtsopts=-N",
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
  createCabalFiles = unlines [
    "{-# START_FILE Setup.hs #-}",
    "import Distribution.Simple",
    "main = defaultMain",
    "",
    "{-# START_FILE test/Spec.hs #-}",
    "main :: IO ()",
    "main = putStrLn \"Test suite not yet implemented\"",
    "",
    "{-# START_FILE src/Lib.hs #-}",
    "module Lib",
    "    ( someFunc",
    "    ) where",
    "",
    "someFunc :: IO ()",
    "someFunc = putStrLn \"someFunc\"",
    "",
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

  -- template for MIT license
  mitLicense :: String
  mitLicense = unlines [
    "{-# START_FILE LICENSE #-}",
    "MIT License",
    "",
    printf "Copyright (c) 2017 %s" nm,
    "",
    "Permission is hereby granted, free of charge, to any person obtaining a copy",
    "of this software and associated documentation files (the \"Software\"), to deal",
    "in the Software without restriction, including without limitation the rights",
    "to use, copy, modify, merge, publish, distribute, sublicense, and/or sell",
    "copies of the Software, and to permit persons to whom the Software is",
    "furnished to do so, subject to the following conditions:",
    "",
    "The above copyright notice and this permission notice shall be included in all",
    "copies or substantial portions of the Software.",
    "",
    "THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR",
    "IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,",
    "FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE",
    "AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER",
    "LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,",
    "OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE",
    "SOFTWARE.",
    ""
    ]

  -- template for BSD2 license
  bsd2License :: String
  bsd2License = bsdStart 2 ++ bsdEnd

  -- template for BSD3 license
  bsd3License :: String
  bsd3License = bsdStart 3 ++ unlines [
    "* Neither the name of the copyright holder nor the names of its",
    "  contributors may be used to endorse or promote products derived from",
    "  this software without specific prior written permission.",
    ""
    ] ++ bsdEnd

  bsdStart :: Int -> String
  bsdStart int = unlines [
    "{-# START_FILE LICENSE #-}",
    printf "BSD %s-Clause License" (show int),
    "",
    printf "Copyright (c) 2017, %s" nm,
    "All rights reserved.",
    "",
    "Redistribution and use in source and binary forms, with or without",
    "modification, are permitted provided that the following conditions are met:",
    "",
    "* Redistributions of source code must retain the above copyright notice, this",
    "  list of conditions and the following disclaimer.",
    "",
    "* Redistributions in binary form must reproduce the above copyright notice,",
    "  this list of conditions and the following disclaimer in the documentation",
    "  and/or other materials provided with the distribution.",
    ""
    ]

  bsdEnd :: String
  bsdEnd = unlines [
    "THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS \"AS IS\"",
    "AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE",
    "IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE",
    "DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE",
    "FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL",
    "DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR",
    "SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER",
    "CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,",
    "OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE",
    "OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.",
    ""
    ]

  -- template of other licenses
  publicDomainLicense :: String
  publicDomainLicense = unlines [
    "{-# START_FILE LICENSE #-}",
    "Creative Commons Legal Code",
    "",
    "CC0 1.0 Universal",
    "",
    "    CREATIVE COMMONS CORPORATION IS NOT A LAW FIRM AND DOES NOT PROVIDE",
    "    LEGAL SERVICES. DISTRIBUTION OF THIS DOCUMENT DOES NOT CREATE AN",
    "    ATTORNEY-CLIENT RELATIONSHIP. CREATIVE COMMONS PROVIDES THIS",
    "    INFORMATION ON AN \"AS-IS\" BASIS. CREATIVE COMMONS MAKES NO WARRANTIES",
    "    REGARDING THE USE OF THIS DOCUMENT OR THE INFORMATION OR WORKS",
    "    PROVIDED HEREUNDER, AND DISCLAIMS LIABILITY FOR DAMAGES RESULTING FROM",
    "    THE USE OF THIS DOCUMENT OR THE INFORMATION OR WORKS PROVIDED",
    "    HEREUNDER.",
    "",
    "Statement of Purpose",
    "",
    "The laws of most jurisdictions throughout the world automatically confer",
    "exclusive Copyright and Related Rights (defined below) upon the creator",
    "and subsequent owner(s) (each and all, an \"owner\") of an original work of",
    "authorship and/or a database (each, a \"Work\").",
    "",
    "Certain owners wish to permanently relinquish those rights to a Work for",
    "the purpose of contributing to a commons of creative, cultural and",
    "scientific works (\"Commons\") that the public can reliably and without fear",
    "of later claims of infringement build upon, modify, incorporate in other",
    "works, reuse and redistribute as freely as possible in any form whatsoever",
    "and for any purposes, including without limitation commercial purposes.",
    "These owners may contribute to the Commons to promote the ideal of a free",
    "culture and the further production of creative, cultural and scientific",
    "works, or to gain reputation or greater distribution for their Work in",
    "part through the use and efforts of others.",
    "",
    "For these and/or other purposes and motivations, and without any",
    "expectation of additional consideration or compensation, the person",
    "associating CC0 with a Work (the \"Affirmer\"), to the extent that he or she",
    "is an owner of Copyright and Related Rights in the Work, voluntarily",
    "elects to apply CC0 to the Work and publicly distribute the Work under its",
    "terms, with knowledge of his or her Copyright and Related Rights in the",
    "Work and the meaning and intended legal effect of CC0 on those rights.",
    "",
    "1. Copyright and Related Rights. A Work made available under CC0 may be",
    "protected by copyright and related or neighboring rights (\"Copyright and",
    "Related Rights\"). Copyright and Related Rights include, but are not",
    "limited to, the following:",
    "",
    "  i. the right to reproduce, adapt, distribute, perform, display,",
    "     communicate, and translate a Work;",
    " ii. moral rights retained by the original author(s) and/or performer(s);",
    "iii. publicity and privacy rights pertaining to a person's image or",
    "     likeness depicted in a Work;",
    " iv. rights protecting against unfair competition in regards to a Work,",
    "     subject to the limitations in paragraph 4(a), below;",
    "  v. rights protecting the extraction, dissemination, use and reuse of data",
    "     in a Work;",
    " vi. database rights (such as those arising under Directive 96/9/EC of the",
    "     European Parliament and of the Council of 11 March 1996 on the legal",
    "     protection of databases, and under any national implementation",
    "     thereof, including any amended or successor version of such",
    "     directive); and",
    "vii. other similar, equivalent or corresponding rights throughout the",
    "     world based on applicable law or treaty, and any national",
    "     implementations thereof.",
    "",
    "2. Waiver. To the greatest extent permitted by, but not in contravention",
    "of, applicable law, Affirmer hereby overtly, fully, permanently,",
    "irrevocably and unconditionally waives, abandons, and surrenders all of",
    "Affirmer's Copyright and Related Rights and associated claims and causes",
    "of action, whether now known or unknown (including existing as well as",
    "future claims and causes of action), in the Work (i) in all territories",
    "worldwide, (ii) for the maximum duration provided by applicable law or",
    "treaty (including future time extensions), (iii) in any current or future",
    "medium and for any number of copies, and (iv) for any purpose whatsoever,",
    "including without limitation commercial, advertising or promotional",
    "purposes (the \"Waiver\"). Affirmer makes the Waiver for the benefit of each",
    "member of the public at large and to the detriment of Affirmer's heirs and",
    "successors, fully intending that such Waiver shall not be subject to",
    "revocation, rescission, cancellation, termination, or any other legal or",
    "equitable action to disrupt the quiet enjoyment of the Work by the public",
    "as contemplated by Affirmer's express Statement of Purpose.",
    "",
    "3. Public License Fallback. Should any part of the Waiver for any reason",
    "be judged legally invalid or ineffective under applicable law, then the",
    "Waiver shall be preserved to the maximum extent permitted taking into",
    "account Affirmer's express Statement of Purpose. In addition, to the",
    "extent the Waiver is so judged Affirmer hereby grants to each affected",
    "person a royalty-free, non transferable, non sublicensable, non exclusive,",
    "irrevocable and unconditional license to exercise Affirmer's Copyright and",
    "Related Rights in the Work (i) in all territories worldwide, (ii) for the",
    "maximum duration provided by applicable law or treaty (including future",
    "time extensions), (iii) in any current or future medium and for any number",
    "of copies, and (iv) for any purpose whatsoever, including without",
    "limitation commercial, advertising or promotional purposes (the",
    "\"License\"). The License shall be deemed effective as of the date CC0 was",
    "applied by Affirmer to the Work. Should any part of the License for any",
    "reason be judged legally invalid or ineffective under applicable law, such",
    "partial invalidity or ineffectiveness shall not invalidate the remainder",
    "of the License, and in such case Affirmer hereby affirms that he or she",
    "will not (i) exercise any of his or her remaining Copyright and Related",
    "Rights in the Work or (ii) assert any associated claims and causes of",
    "action with respect to the Work, in either case contrary to Affirmer's",
    "express Statement of Purpose.",
    "",
    "4. Limitations and Disclaimers.",
    "",
    " a. No trademark or patent rights held by Affirmer are waived, abandoned,",
    "    surrendered, licensed or otherwise affected by this document.",
    " b. Affirmer offers the Work as-is and makes no representations or",
    "    warranties of any kind concerning the Work, express, implied,",
    "    statutory or otherwise, including without limitation warranties of",
    "    title, merchantability, fitness for a particular purpose, non",
    "    infringement, or the absence of latent or other defects, accuracy, or",
    "    the present or absence of errors, whether or not discoverable, all to",
    "    the greatest extent permissible under applicable law.",
    " c. Affirmer disclaims responsibility for clearing rights of other persons",
    "    that may apply to the Work or any use thereof, including without",
    "    limitation any person's Copyright and Related Rights in the Work.",
    "    Further, Affirmer disclaims responsibility for obtaining any necessary",
    "    consents, permissions or other rights required for any use of the",
    "    Work.",
    " d. Affirmer understands and acknowledges that Creative Commons is not a",
    "    party to this document and has no duty or obligation with respect to",
    "    this CC0 or use of the Work."
    ]
