{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}

module Summoner.Template.GitHub
       ( gitHubFiles
       ) where

import Data.List (delete)
import NeatInterpolation (text)

import Summoner.Default (defaultGHC)
import Summoner.GhcVer (GhcVer (..), showGhcVer)
import Summoner.Settings (Settings (..))
import Summoner.Text (tconcatMap)
import Summoner.Tree (TreeFs (..))


gitHubFiles :: Settings -> [TreeFs]
gitHubFiles Settings{..} =
    [File ".gitignore" gitignore     | settingsGithub]
 ++ [File ".travis.yml" travisYml    | settingsTravis]
 ++ [File "appveyor.yml" appVeyorYml | settingsAppVey]
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
        $endLine
        |]


    -- create travis.yml template
    travisYml :: Text
    travisYml =
        let travisStackMtr = memptyIfFalse settingsStack $
                tconcatMap travisStackMatrixItem (delete defaultGHC (toList settingsTestedVersions))
                    <> travisStackMatrixDefaultItem
            travisCabalMtr = memptyIfFalse settingsCabal $
                tconcatMap travisCabalMatrixItem (toList settingsTestedVersions)
            installAndScript =
                if settingsCabal
                then if settingsStack
                     then installScriptBoth
                     else installScriptCabal
                else installScriptStack
            travisCabalCache = memptyIfFalse settingsCabal "- \"$HOME/.cabal\""
            travisStackCache = memptyIfFalse settingsStack
                [text|
                - "$$HOME/.stack"
                - "$$TRAVIS_BUILD_DIR/.stack-work"
                |]
        in
        [text|
        sudo: true
        language: haskell

        git:
          depth: 5

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
        $endLine
        |]

    cabalTest :: Text
    cabalTest = if settingsTest then "cabal new-test" else "echo 'No tests'"

    travisCabalMatrixItem :: GhcVer -> Text
    travisCabalMatrixItem (showGhcVer -> ghcV) =
        [text|
        - ghc: ${ghcV}
          env: GHCVER='${ghcV}' CABALVER='head'
          os: linux
          addons:
            apt:
              sources:
              - hvr-ghc
              packages:
              - ghc-${ghcV}
              - cabal-install-head
        $endLine
        |]

    travisStackMatrixItem :: GhcVer -> Text
    travisStackMatrixItem (showGhcVer -> ghcV) =
        [text|
        - ghc: ${ghcV}
          env: GHCVER='${ghcV}' STACK_YAML="$$TRAVIS_BUILD_DIR/stack-$$GHCVER.yaml"
          os: linux
          addons:
            apt:
              packages:
              - libgmp-dev
          $endLine
        |]

    travisStackMatrixDefaultItem :: Text
    travisStackMatrixDefaultItem = let defGhc = showGhcVer defaultGHC in
        [text|
        - ghc: ${defGhc}
          env: GHCVER='${defGhc}' STACK_YAML="$$TRAVIS_BUILD_DIR/stack.yaml"
          os: linux
          addons:
            apt:
              packages:
              - libgmp-dev
        $endLine
        |]

    installScriptBoth :: Text
    installScriptBoth =
        [text|
        install:
          - |
            if [ -z "$$STACK_YAML" ]; then
              export PATH="/opt/ghc/$$GHCVER/bin:/opt/cabal/$$CABALVER/bin:$$PATH"
              echo $$PATH
              cabal new-update
              cabal new-build --enable-tests --enable-benchmarks
            else
              mkdir -p ~/.local/bin
              export PATH="$$HOME/.local/bin:$$PATH"
              travis_retry curl -L 'https://www.stackage.org/stack/linux-x86_64' | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
              stack --version
              stack setup --no-terminal --install-cabal 2.0.1.0
              stack ghc -- --version
              stack build --only-dependencies --no-terminal
            fi
        script:
          - |
            if [ -z "$$STACK_YAML" ]; then
               ${cabalTest}
            else
              stack build --test --bench --no-run-benchmarks --no-terminal --ghc-options=-Werror
            fi
        $endLine
        |]

    installScriptCabal :: Text
    installScriptCabal =
        [text|
        install:
          - export PATH="/opt/ghc/$$GHCVER/bin:/opt/cabal/$$CABALVER/bin:$$PATH"
          - echo $$PATH
          - cabal new-update
          - cabal new-build --enable-tests --enable-benchmarks
        script:
          - ${cabalTest}
        $endLine
        |]

    installScriptStack :: Text
    installScriptStack =
        [text|
        install:
          - mkdir -p ~/.local/bin
          - export PATH="$$HOME/.local/bin:$$PATH"
          - travis_retry curl -L 'https://www.stackage.org/stack/linux-x86_64' | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
          - stack --version
          - stack setup --no-terminal --install-cabal 2.0.1.0
          - stack ghc -- --version
          - stack build --only-dependencies --no-terminal
        script:
          - stack build --test --bench --no-run-benchmarks --no-terminal --ghc-options=-Werror
        $endLine
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
