{-# LANGUAGE QuasiQuotes  #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module contains functions for stack template creation.

module Summoner.Template
       ( createProjectTemplate
       ) where

import Relude

import Data.List (delete)
import NeatInterpolation (text)

import Summoner.Default (defaultGHC, endLine)
import Summoner.GhcVer (GhcVer (..), baseVer, latestLts, showGhcVer)
import Summoner.License (License (..))
import Summoner.ProjectData (CustomPrelude (..), ProjectData (..))
import Summoner.Text (intercalateMap, packageToModule)
import Summoner.Tree (TreeFs (..))

import qualified Data.Text as T

----------------------------------------------------------------------------
-- Project Directory Creation
----------------------------------------------------------------------------

memptyIfFalse :: Monoid m => Bool -> m -> m
memptyIfFalse p val = if p then val else mempty

-- | Creating tree structure of the project.
createProjectTemplate :: ProjectData -> TreeFs
createProjectTemplate ProjectData{..} = Dir (toString repo) $
    [ File (toString repo <> ".cabal")
           ( createCabalTop
          <> memptyIfFalse github createCabalGit
          <> memptyIfFalse isLib createCabalLib
          <> memptyIfFalse isExe
                        ( createCabalExe
                        $ memptyIfFalse isLib $ ", " <> repo )
          <> memptyIfFalse test
                        ( createCabalTest
                        $ memptyIfFalse isLib $ ", " <> repo )
          <> memptyIfFalse bench
                        ( createCabalBenchmark
                        $ memptyIfFalse isLib $ ", " <> repo )
           )
    , File "README.md" readme
    , File "CHANGELOG.md" changelog
    , File "LICENSE" $ unLicense licenseText
    ]
 ++ createCabalFiles
 ++ memptyIfFalse stack (createStackYamls testedVersions)
 ++ [File ".gitignore" gitignore | github]
 ++ [File ".travis.yml" travisYml | travis]
 ++ [File "appveyor.yml" appVeyorYml | appVey]
 ++ maybe [] (\x -> [File ".stylish-haskell.yaml" x]) stylish
 ++ maybe [] (\x -> [File "CONTRIBUTING.md" x]) contributing
  where
    license = show licenseName

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
        cabal-version:       1.24
        name:                $repo
        version:             0.0.0
        description:         $description
        synopsis:            $description
        $githubHomepage
        $githubBugReports
        license:             $license
        license-file:        LICENSE
        author:              $nm
        maintainer:          $email
        copyright:           $year $nm
        category:            $category
        build-type:          Simple
        extra-doc-files:     README.md
                           , CHANGELOG.md
        tested-with:         $testedGhcs
        $endLine
        |]
      where
        githubHomepage :: Text  =  memptyIfFalse github $ "homepage:            https://github.com/" <> owner <> "/" <> repo
        githubBugReports :: Text = memptyIfFalse github $ "bug-reports:         https://github.com/" <> owner <> "/" <> repo <> "/issues"


    testedGhcs :: Text
    testedGhcs = intercalateMap ", " (mappend "GHC == " . showGhcVer) testedVersions

    defaultExtensions :: Text
    defaultExtensions = case extensions of
        [] -> ""
        xs -> "default-extensions:  " <> T.intercalate "\n                     " xs

    createCabalGit :: Text
    createCabalGit =
        [text|
        source-repository head
          type:                git
          location:            https://github.com/${owner}/${repo}.git
        $endLine
        |]

    ghcOptions :: Text
    ghcOptions = case warnings of
        [] -> defaultWarnings
        xs -> T.intercalate "\n" xs

    defaultWarnings :: Text
    defaultWarnings =
        [text|
        -Wincomplete-uni-patterns
        -Wincomplete-record-updates
        -Wcompat
        -Widentities
        $versionWarnings
        |]

    versionWarnings :: Text
    versionWarnings
        =  memptyIfFalse (testedVersions `hasLeast` Ghc801)
            "-Wredundant-constraints\n"
        <> memptyIfFalse (testedVersions `hasLeast` Ghc822)
            "-fhide-source-paths\n"
        <> memptyIfFalse (testedVersions `hasLeast` Ghc843)
            [text|
            -Wmissing-export-lists
            -Wpartial-fields
            |]
      where
        hasLeast list el = all (>= el) list

    createCabalLib :: Text
    createCabalLib =
        [text|
        library
          hs-source-dirs:      src
          exposed-modules:     $libModuleName
                               $preludeMod
          ghc-options:         -Wall
                               $ghcOptions
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
          ghc-options:         -Wall
                               -threaded
                               -rtsopts
                               -with-rtsopts=-N
                               $ghcOptions
          build-depends:       $base
                             $r
                             $customPreludePack
          default-language:    Haskell2010
          $defaultExtensions
        $endLine
        |]

    createCabalTest :: Text -> Text
    createCabalTest r =
        [text|
        test-suite ${repo}-test
          type:                exitcode-stdio-1.0
          hs-source-dirs:      test
          main-is:             Spec.hs
          ghc-options:         -Wall
                               -threaded
                               -rtsopts
                               -with-rtsopts=-N
                               $ghcOptions
          build-depends:       $base
                             $r
                             $customPreludePack
          default-language:    Haskell2010
          $defaultExtensions
        $endLine
        |]

    createCabalBenchmark :: Text -> Text
    createCabalBenchmark r =
        [text|
        benchmark ${repo}-benchmark
          type:                exitcode-stdio-1.0
          hs-source-dirs:      benchmark
          main-is:             Main.hs
          ghc-options:         -Wall
                               -threaded
                               -rtsopts
                               -with-rtsopts=-N
                               $ghcOptions
          build-depends:       $base
                             , gauge
                             $r
                             $customPreludePack
          default-language:    Haskell2010
          $defaultExtensions
        $endLine
        |]

    createCabalFiles :: [TreeFs]
    createCabalFiles =
        [ Dir "app"       [exeFile]               | isExe ]
     ++ [ Dir "test"      [testFile]              | test  ]
     ++ [ Dir "benchmark" [benchmarkFile]         | bench ]
     ++ [ Dir "src"       $ libFile : preludeFile | isLib ]

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
        main :: IO ()
        main = putStrLn ("Hello, world!" :: String)
        $endLine
        |]

    createExe :: Text
    createExe =
        [text|
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
    readme = T.intercalate "\n" $
        [ "# " <> repo
        , ""
        , hackage
        , licenseBadge
        ]
     ++ [stackLtsBadge | stack]
     ++ [stackNightlyBadge | stack]
     ++ [travisBadge | travis]
     ++ [appVeyorBadge | appVey]
     ++ [""
        , description
        ]
      where
        hackageShield :: Text =
            "https://img.shields.io/hackage/v/" <> repo <> ".svg"
        hackageLink :: Text =
            "https://hackage.haskell.org/package/" <> repo
        hackage :: Text = makeBadge "Hackage" hackageShield hackageLink

        licenseShield :: Text =
            "https://img.shields.io/badge/license-" <> T.replace "-" "--" license <> "-blue.svg"
        licenseBadge :: Text =
            makeBadge (license <> " license") licenseShield "LICENSE"

        stackShieldLts :: Text =
            "http://stackage.org/package/" <> repo <> "/badge/lts"
        stackLinkLts :: Text =
            "http://stackage.org/lts/package/" <> repo

        stackShieldNightly :: Text =
            "http://stackage.org/package/" <> repo <> "/badge/nightly"
        stackLinkNightly :: Text =
            "http://stackage.org/nightly/package/" <> repo
        stackLtsBadge :: Text =
            makeBadge "Stackage Lts" stackShieldLts stackLinkLts
        stackNightlyBadge :: Text =
            makeBadge "Stackage Nightly" stackShieldNightly stackLinkNightly

        travisShield :: Text =
            "https://secure.travis-ci.org/" <> owner <> "/" <> repo <> ".svg"
        travisLink :: Text =
            "https://travis-ci.org/" <> owner <> "/" <> repo
        travisBadge :: Text =
            makeBadge "Build status" travisShield travisLink

        appVeyorShield :: Text =
            "https://ci.appveyor.com/api/projects/status/github/" <> owner <> "/" <> repo <> "?branch=master&svg=true"
        appVeyorLink :: Text =
            "https://ci.appveyor.com/project/" <> owner <> "/" <> repo
        appVeyorBadge :: Text =
            makeBadge "Windows build status" appVeyorShield appVeyorLink

        makeBadge :: Text -> Text -> Text -> Text
        makeBadge title shield link = "[![" <> title <> "](" <> shield <> ")](" <> link <> ")"

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
        # Change log

        `$repo` uses [PVP Versioning][1].
        $githubLine

        0.0.0
        =====

        * Initially created.

        [1]: https://pvp.haskell.org
        $githubFootNote
        |]
      where
        githubLine :: Text = memptyIfFalse github $ "The change log is available [on GitHub][2]."
        githubFootNote :: Text = memptyIfFalse github $
            "[2]: https://github.com/" <> owner <> "/" <> repo <> "/releases"

    -- create travis.yml template
    travisYml :: Text
    travisYml =
        let travisStackMtr = memptyIfFalse stack $
                T.concat (map travisStackMatrixItem $ delete defaultGHC testedVersions)
                    <> travisStackMatrixDefaultItem
            travisCabalMtr = memptyIfFalse cabal $
                T.concat $ map travisCabalMatrixItem testedVersions
            installAndScript =
                if cabal
                then if stack
                     then installScriptBoth
                     else installScriptCabal
                else installScriptStack
            travisCabalCache = memptyIfFalse cabal "- \"$HOME/.cabal\""
            travisStackCache = memptyIfFalse stack
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
    cabalTest = if test then "cabal new-test" else "echo 'No tests'"

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
              stack build --test --bench --no-run-benchmarks --no-terminal
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
          - stack build --test --bench --no-run-benchmarks --no-terminal
        $endLine
        |]

    -- create @stack.yaml@ file with LTS corresponding to specified ghc version
    createStackYamls :: [GhcVer] -> [TreeFs]
    createStackYamls = map createStackYaml
      where
        createStackYaml :: GhcVer -> TreeFs
        createStackYaml ghcV = let ver = case ghcV of
                                      Ghc843 -> ""
                                      _      -> "-" <> showGhcVer ghcV
            in stackYaml ver (latestLts ghcV) (baseVer ghcV)
          where
            stackYaml :: Text -> Text -> Text -> TreeFs
            stackYaml ghc lts baseV = File (toString $ "stack" <> ghc <> ".yaml")
                [text|
                resolver: lts-${lts}

                $extraDeps

                $ghcOpts
                $endLine
                |]
              where
                extraDeps :: Text
                extraDeps = case prelude of
                    Nothing -> ""
                    Just _  -> "extra-deps: [base-noprelude-" <> baseV <> "]"
                ghcOpts :: Text
                ghcOpts = if ghcV <= Ghc802 then
                            ""
                          else
                            [text|
                            ghc-options:
                              "$$locals": -fhide-source-paths
                            |]


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
