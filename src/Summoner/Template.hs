{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains functions for stack template creation.

module Summoner.Template
       ( createStackTemplate
       ) where

import Universum

import NeatInterpolation (text)

import Summoner.Default (defaultGHC, endLine)
import Summoner.ProjectData (CustomPrelude (..), GhcVer (..), ProjectData (..), baseNopreludeVer,
                             latestLts, showGhcVer)
import Summoner.Text (intercalateMap, packageToModule)
import Summoner.Tree (TreeFs (..))

import qualified Data.Text as T

----------------------------------------------------------------------------
-- Stack File Creation
----------------------------------------------------------------------------

emptyIfNot :: Bool -> Text -> Text
emptyIfNot p txt = if p then txt else ""

-- | Creating template file to use in `stack new` command
createStackTemplate :: ProjectData ->  TreeFs
createStackTemplate ProjectData{..} = Dir (toString repo) $
    [ File (toString repo <> ".cabal")
           ( createCabalTop
          <> emptyIfNot isLib createCabalLib
          <> emptyIfNot isExe
                        ( createCabalExe
                        $ emptyIfNot isLib $ ", " <> repo )
          <> emptyIfNot test createCabalTest
          <> emptyIfNot bench
                        ( createCabalBenchmark
                        $ emptyIfNot isLib $ ", " <> repo )
          <> emptyIfNot github createCabalGit
           )
    , File "README.md" readme
    , File "CHANGELOG.md" changelog
    , File "LICENSE" licenseText
    ]
 ++ createCabalFiles
 ++ createStackYamls testedVersions
 ++ [File ".gitignore" gitignore | github]
 ++ [File ".travis.yml" travisYml | travis]
 ++ [File "appveyor.yml" appVeyorYml | appVey]
 ++ [File "b" scriptSh | script]
  where
    -- Creates module name from the name of the project
    libModuleName :: Text
    libModuleName = packageToModule repo

    preludeMod :: Text
    preludeMod = case prelude of
        Nothing -> ""
        Just _  -> "Prelude"

    customPreludePack :: Text
    customPreludePack = case prelude of
        Nothing          -> ""
        Just Prelude{..} -> ", " <> cpPackage

    -- all basic project information for `*.cabal` file
    createCabalTop :: Text
    createCabalTop =
        [text|
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
                           , CHANGELOG.md
        cabal-version:       1.24
        tested-with:         $testedGhcs
        $endLine
        |]

    testedGhcs :: Text
    testedGhcs = intercalateMap ", " (mappend "GHC == " . showGhcVer) testedVersions

    defaultExtensions :: Text
    defaultExtensions = case extensions of
        [] -> ""
        xs -> "default-extensions:  " <> T.intercalate "\n                     " xs

    createCabalLib :: Text
    createCabalLib =
        [text|
        library
          hs-source-dirs:      src
          exposed-modules:     $libModuleName
                               $preludeMod
          ghc-options:         -Wall
          build-depends:       $base
                             $customPreludePack
          default-language:    Haskell2010
          $defaultExtensions
        $endLine
        |]

    createCabalExe :: Text -> Text
    createCabalExe r =
        [text|
        executable $repo
          hs-source-dirs:      app
          main-is:             Main.hs
          ghc-options:         -Wall -threaded -rtsopts -with-rtsopts=-N
          build-depends:       $base
                             $r
                             $customPreludePack
          default-language:    Haskell2010
          $defaultExtensions
        $endLine
        |]

    createCabalTest :: Text
    createCabalTest =
        [text|
        test-suite ${repo}-test
          type:                exitcode-stdio-1.0
          hs-source-dirs:      test
          main-is:             Spec.hs
          build-depends:       $base
                             , $repo
                             $customPreludePack
          ghc-options:         -Wall -Werror -threaded -rtsopts -with-rtsopts=-N
          default-language:    Haskell2010
          $defaultExtensions
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
          build-depends:       $base
                             , gauge
                             $customPreludePack
                             $r
          $defaultExtensions
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

    createCabalFiles :: [TreeFs]
    createCabalFiles =
        [ Dir "app"       [exeFile]                | isExe ]
     ++ [ Dir "test"      [testFile]               | test  ]
     ++ [ Dir "benchmark" [benchmarkFile]          | bench ]
     ++ [ Dir "src"     $ [libFile] ++ preludeFile | isLib ]

    testFile :: TreeFs
    testFile = File "Spec.hs"
        [text|
        main :: IO ()
        main = putStrLn ("Test suite not yet implemented" :: String)
        $endLine
        |]

    libFile :: TreeFs
    libFile = File (toString libModuleName <> ".hs")
        [text|
        module $libModuleName
               ( someFunc
               ) where

        someFunc :: IO ()
        someFunc = putStrLn ("someFunc" :: String)
        $endLine
        |]

    preludeFile :: [TreeFs]
    preludeFile = case prelude of
        Nothing -> []
        Just Prelude{..} -> one $ File "Prelude.hs"
            [text|
            -- | Uses [$cpPackage](https://hackage.haskell.org/package/${cpPackage}) as default Prelude.

            module Prelude
                   ( module $cpModule
                   ) where

            import $cpModule
            $endLine
            |]

    exeFile :: TreeFs
    exeFile = File "Main.hs" $ if isLib then createExe else createOnlyExe

    createOnlyExe :: Text
    createOnlyExe =
        [text|
        module Main where

        main :: IO ()
        main = putStrLn ("Hello, world!" :: String)
        $endLine
        |]

    createExe :: Text
    createExe =
        [text|
        module Main where

        import $libModuleName (someFunc)

        main :: IO ()
        main = someFunc
        $endLine
        |]

    benchmarkFile :: TreeFs
    benchmarkFile = File "Main.hs"
      [text|
      import Gauge.Main

      main :: IO ()
      main = defaultMain [bench "const" (whnf const ())]
      $endLine
      |]

    -- create README template
    readme :: Text
    readme =
        [text|
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
        .ghc.environment.*
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

    -- create travis.yml template
    travisYml :: Text
    travisYml =
        let travisMtr = T.concat (map (travisMatrixItem . showGhcVer) testedVersions)
            defGhc    = showGhcVer defaultGHC in
        [text|
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

          - ghc: $defGhc
            env: GHCVER='${defGhc}' STACK_YAML="$$HOME/build/${owner}/${repo}/stack.yaml"

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

    -- create @stack.yaml@ file with LTS corresponding to specified ghc version
    createStackYamls :: [GhcVer] -> [TreeFs]
    createStackYamls = map createStackYaml
      where
        createStackYaml :: GhcVer -> TreeFs
        createStackYaml ghcV = let ver = case ghcV of
                                      Ghc822 -> ""
                                      _      -> "-" <> showGhcVer ghcV
            in stackYaml ver (latestLts ghcV) (baseNopreludeVer ghcV)
          where
            stackYaml :: Text -> Text -> Text -> TreeFs
            stackYaml ghc lts baseVer = File (toString $ "stack" <> ghc <> ".yaml")
                [text|
                resolver: lts-${lts}

                $extraDeps

                ghc-options:
                  "$$locals": -fhide-source-paths
                $endLine
                |]
              where
                extraDeps :: Text
                extraDeps = case prelude of
                    Nothing -> ""
                    Just _  -> "extra-deps: [base-noprelude-" <> baseVer <> "]"


    -- create appveyor.yml template
    appVeyorYml :: Text
    appVeyorYml =
        [text|
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
