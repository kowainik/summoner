{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}

{-|
Copyright: (c) 2017-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module contains template for GitHub related docs:

 * @.gitignore@ — static file with all Haskell related ignored files.
 * @appveyor.yml@ — Appveyor CI for Stack project only.
 * @.travis.yml@ — depending on the build tool and supported GHC versions
   builds the Travis matrix with all necessary checks, including HLint check.
   __NOTE:__ Old GHC versions is not included into Travis matrix for Stack due to
   Stack limitations with the Cabal version usage on each GHC. See this issue to
   track the problem:

    + https://github.com/commercialhaskell/stack/issues/4488
-}

module Summoner.Template.GitHub
       ( gitHubFiles
       ) where

import Data.List ((\\))
import NeatInterpolation (text)

import Summoner.Default (defaultGHC)
import Summoner.GhcVer (GhcVer (..), oldGhcs, showGhcVer)
import Summoner.Settings (Settings (..))
import Summoner.Template.Mempty (memptyIfFalse)
import Summoner.Text (endLine, intercalateMap, tconcatMap)
import Summoner.Tree (TreeFs (..))


gitHubFiles :: Settings -> [TreeFs]
gitHubFiles Settings{..} = concat
    [ [File ".gitignore" (gitignoreDefault <> gitignoreCustom) | settingsGitHub]
    , [Dir ".github" [ Dir "workflows" [ File "ci.yml" ghActionsYml ]] | settingsGhActions ]
    , [File ".travis.yml" travisYml    | settingsTravis]
    , [File "appveyor.yml" appVeyorYml | settingsAppVeyor]
    ]
  where
    -- default .gitignore template
    gitignoreDefault :: Text
    gitignoreDefault =
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
        stack.yaml.lock

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

    -- additional .gitignore
    gitignoreCustom :: Text
    gitignoreCustom = if null settingsGitignore
        then ""
        else unlines ("\n# User specific" : settingsGitignore)

    ghActionsYml :: Text
    ghActionsYml = [text|
        name: CI

        on:
          # Trigger the workflow on push or pull request,
          # but only for the master branch
          push:
            branches:
              - master
          pull_request:

        jobs:
          build:
            name: ghc ${{ matrix.ghc }}
            runs-on: ubuntu-16.04
            strategy:
              matrix:
                ghc: ${ghActionsVersions}
                cabal: ["3.0"]

            steps:
            - uses: actions/checkout@v2
              if: github.event.action == 'opened' || github.event.action == 'synchronize' || github.event.ref == 'refs/heads/master'

            - uses: actions/setup-haskell@v1
              name: Setup Haskell
              with:
                ghc-version: ${{ matrix.ghc }}
                cabal-version: ${{ matrix.cabal }}

            - uses: actions/cache@v1
              name: Cache ~/.cabal/store
              with:
                path: ~/.cabal/store
                key: ${{ runner.os }}-${{ matrix.ghc }}-cabal

            - name: Build
              run: |
                cabal new-update
                cabal new-build --enable-tests --enable-benchmarks --write-ghc-environment-files=always

            - name: Test
              run: |
                ${cabalTest}
        |]


    ghActionsVersions :: Text
    ghActionsVersions = memptyIfFalse settingsGhActions $
      "[" <> intercalateMap ", " ghActionsMatrixItem settingsTestedVersions <> "]"

    ghActionsMatrixItem :: GhcVer -> Text
    ghActionsMatrixItem v = "\"" <> showGhcVer v <> "\""

    -- create travis.yml template
    travisYml :: Text
    travisYml =
        [text|
        sudo: true
        language: haskell

        git:
          depth: 5

        cabal: "2.4"

        cache:
          directories:
          $travisCabalCache
          $travisStackCache

        matrix:
          include:
          $travisCabalMtr
          $travisStackMtr

        $installAndScript

        notifications:
          email: false
        |]

    travisCabalCache, travisStackCache :: Text
    travisCabalCache = memptyIfFalse settingsCabal "- \"$HOME/.cabal/store\""
    travisStackCache = memptyIfFalse settingsStack
        [text|
        - "$$HOME/.stack"
        - "$$TRAVIS_BUILD_DIR/.stack-work"
        |]


    cabalTest :: Text
    cabalTest = if settingsTest
        then "cabal new-test --enable-tests"
        else "echo 'No tests'"

    travisCabalMtr :: Text
    travisCabalMtr = memptyIfFalse settingsCabal $
        tconcatMap travisCabalMatrixItem settingsTestedVersions

    travisCabalMatrixItem :: GhcVer -> Text
    travisCabalMatrixItem (showGhcVer -> ghcV) = [text|- ghc: $ghcV|]

    -- Due to Stach issues with newer Cabal versions we are not supporting Travis CI for GHC <= 8.0.2 for stack
    travisStackMtr :: Text
    travisStackMtr = memptyIfFalse settingsStack $ tconcatMap
           travisStackMatrixItem (settingsTestedVersions \\ (defaultGHC:oldGhcs))
        <> travisStackMatrixDefaultItem

    travisStackMatrixItem :: GhcVer -> Text
    travisStackMatrixItem (showGhcVer -> ghcV) =
        [text|
        $endLine
        - ghc: ${ghcV}
          env: STACK_YAML="$$TRAVIS_BUILD_DIR/stack-$ghcV.yaml"
        |]

    travisStackMatrixDefaultItem :: Text
    travisStackMatrixDefaultItem = let defGhc = showGhcVer defaultGHC in
        [text|
        $endLine
        - ghc: ${defGhc}
          env: STACK_YAML="$$TRAVIS_BUILD_DIR/stack.yaml"
        |]

    installAndScript :: Text
    installAndScript =
        if settingsCabal
        then if settingsStack
             then installScriptBoth
             else installScriptCabal
        else installScriptStack

    installScriptBoth :: Text
    installScriptBoth =
        [text|
        install:
          $hlintCheck

          - |
            if [ -z "$$STACK_YAML" ]; then
              ghc --version
              cabal --version
              cabal new-update
              cabal new-build --enable-tests --enable-benchmarks
            else
              curl -sSL https://get.haskellstack.org/ | sh
              stack --version
              stack build --system-ghc --test --bench --no-run-tests --no-run-benchmarks --ghc-options=-Werror
            fi

        script:
          - |
            if [ -z "$$STACK_YAML" ]; then
               ${cabalTest}
            else
              stack test --system-ghc
            fi
        |]

    installScriptCabal :: Text
    installScriptCabal =
        [text|
        install:
          $hlintCheck

          - cabal new-update
          - cabal new-build --enable-tests --enable-benchmarks

        script:
          - ${cabalTest}
        |]

    installScriptStack :: Text
    installScriptStack =
        [text|
        install:
          $hlintCheck

          - curl -sSL https://get.haskellstack.org/ | sh
          - stack --version
          - stack build --system-ghc --test --bench --no-run-tests --no-run-benchmarks --ghc-options=-Werror

        script:
          - stack test --system-ghc
        |]

    hlintCheck :: Text
    hlintCheck =
        [text|
        # HLint check
        - curl -sSL https://raw.github.com/ndmitchell/neil/master/misc/travis.sh | sh -s -- hlint .
        |]

    appVeyorYml :: Text
    appVeyorYml =
        if settingsCabal
        then appVeyorYmlCabal
        else appVeyorYmlStack

    appVeyorYmlCabal :: Text
    appVeyorYmlCabal = let defGhc = showGhcVer defaultGHC in
        [text|
        clone_folder: "c:\\WORK"
        clone_depth: 5

        # Do not build feature branch with open Pull Requests
        skip_branch_with_pr: true

        platform:
          - x86_64

        cache:
          - "C:\\SR"
          - dist-newstyle

        environment:
          global:
            CABOPTS: --store-dir=C:\\SR

          matrix:
            - GHCVER: $defGhc

        install:
          - choco source add -n mistuke -s https://www.myget.org/F/mistuke/api/v2
          - choco install -y cabal --version 2.4.1.0
          - choco install -y ghc   --version $defGhc
          - refreshenv

        before_build:
          - cabal --version
          - ghc   --version
          - cabal %CABOPTS% v2-update

        build_script:
          - cabal %CABOPTS% v2-build --enable-tests
          - cabal %CABOPTS% v2-test  --enable-tests
        |]

    -- create appveyor.yml template
    appVeyorYmlStack :: Text
    appVeyorYmlStack =
        [text|
        clone_depth: 5

        # Do not build feature branch with open Pull Requests
        skip_branch_with_pr: true

        environment:
          STACK_ROOT: C:\sr
          STACK_VERSION: 2.1.1

          # Workaround a gnarly bug https://github.com/haskell/cabal/issues/5386
          # See: https://www.fpcomplete.com/blog/2018/06/sed-a-debugging-story
          # TODO: check if it's fixed once we switch to lst-13 and GHC 8.6
          TMP: "c:\\tmp"

          matrix:
            - STACK_YAML: stack.yaml

        cache:
          - "%STACK_ROOT% -> %STACK_YAML%, appveyor.yml"
          - ".stack-work -> %STACK_YAML%, appveyor.yml"

        install:
          - choco install -y haskell-stack --version %STACK_VERSION%
          - stack setup > nul

        build_script:
          - stack build --test --bench --no-run-tests --no-run-benchmarks

        test_script:
          - stack test
        |]
