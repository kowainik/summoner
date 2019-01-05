{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}

{-| This module contains template for GitHub related docs:

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

import Summoner.Default (defaultCabal, defaultGHC)
import Summoner.GhcVer (GhcVer (..), oldGhcs, showGhcVer)
import Summoner.Settings (Settings (..))
import Summoner.Text (tconcatMap)
import Summoner.Tree (TreeFs (..))


gitHubFiles :: Settings -> [TreeFs]
gitHubFiles Settings{..} =
    [File ".gitignore" gitignore     | settingsGitHub]
 ++ [File ".travis.yml" travisYml    | settingsTravis]
 ++ [File "appveyor.yml" appVeyorYml | settingsAppVeyor]
  where
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
        |]


    -- create travis.yml template
    travisYml :: Text
    travisYml =
        [text|
        sudo: true
        language: haskell

        git:
          depth: 5

        cabal: "$defaultCabal"

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
    cabalTest = if settingsTest then "cabal new-test" else "echo 'No tests'"

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
          - |
            if [ -z "$$STACK_YAML" ]; then
              cabal new-update
              cabal new-build --enable-tests --enable-benchmarks
            else
              curl -sSL https://get.haskellstack.org/ | sh
              stack --version
              stack build --system-ghc --test --no-run-tests
            fi

        script:
          - |
            if [ -z "$$STACK_YAML" ]; then
               ${cabalTest}
            else
              stack build --system-ghc --test --bench --no-run-benchmarks --no-terminal --ghc-options=-Werror
            fi

          # HLint check
          - curl -sSL https://raw.github.com/ndmitchell/neil/master/misc/travis.sh | sh -s -- hlint .
        |]

    installScriptCabal :: Text
    installScriptCabal =
        [text|
        install:
          - cabal new-update
          - cabal new-build --enable-tests --enable-benchmarks

        script:
          - ${cabalTest}
        |]

    installScriptStack :: Text
    installScriptStack =
        [text|
        install:
          - curl -sSL https://get.haskellstack.org/ | sh
          - stack --version
          - stack build --system-ghc --test --no-run-tests
        script:
          - stack build --system-ghc --test --bench --no-run-benchmarks --no-terminal --ghc-options=-Werror
        |]



    -- create appveyor.yml template
    appVeyorYml :: Text
    appVeyorYml =
        [text|
        build: off

        before_test:
        # http://help.appveyor.com/discussions/problems/6312-curl-command-not-found
        - set PATH=C:\Program Files\Git\mingw64\bin;%PATH%

        - curl -sS -ostack.zip -L --insecure http://www.stackage.org/stack/windows-x86_64
        - 7z x stack.zip stack.exe

        clone_folder: "c:\\stack"
        environment:
          global:
            STACK_ROOT: "c:\\sr"

        test_script:
        - stack setup > nul
        # The ugly echo "" hack is to avoid complaints about 0 being an invalid file
        # descriptor
        - echo "" | stack --arch x86_64 --no-terminal build --bench --no-run-benchmarks --test
        |]
