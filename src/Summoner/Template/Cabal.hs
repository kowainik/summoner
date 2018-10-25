{-# LANGUAGE QuasiQuotes #-}

module Summoner.Template.Cabal
       ( cabalFile
       ) where

import NeatInterpolation (text)

import Summoner.GhcVer (GhcVer (..), showGhcVer)
import Summoner.Settings (CustomPrelude (..), Settings (..))
import Summoner.Text (intercalateMap, packageToModule)
import Summoner.Tree (TreeFs (..))

import qualified Data.Text as T


cabalFile :: Settings -> TreeFs
cabalFile Settings{..} = File (toString settingsRepo ++ ".cabal") cabalFileContent
  where
    cabalFileContent :: Text
    cabalFileContent = mconcat
        [ cabalHeader
        , memptyIfFalse settingsGithub sourceRepository
        , memptyIfFalse settingsIsLib   libraryStanza
        , memptyIfFalse settingsIsExe $ executableStanza $ memptyIfFalse settingsIsLib $ ", " <> settingsRepo
        , memptyIfFalse settingsTest  $ testSuiteStanza  $ memptyIfFalse settingsIsLib $ ", " <> settingsRepo
        , memptyIfFalse settingsBench $ benchmarkStanza  $ memptyIfFalse settingsIsLib $ ", " <> settingsRepo
        ]

    -- TODO: do something to not have empty lines
    cabalHeader :: Text
    cabalHeader =
        [text|
        cabal-version:       2.0
        name:                $settingsRepo
        version:             0.0.0
        synopsis:            $settingsDescription
        description:         $settingsDescription
        $githubHomepage
        $githubBugReports
        license:             $licenseName
        license-file:        LICENSE
        author:              $settingsFullName
        maintainer:          $settingsEmail
        copyright:           $settingsYear $settingsFullName
        category:            $settingsCategories
        build-type:          Simple
        extra-doc-files:     README.md
                           , CHANGELOG.md
        tested-with:         $testedGhcs
        $endLine
        |]

    githubHomepage, githubBugReports :: Text
    githubHomepage   = memptyIfFalse settingsGithub $ "homepage:            https://github.com/" <> settingsOwner <> "/" <> settingsRepo
    githubBugReports = memptyIfFalse settingsGithub $ "bug-reports:         https://github.com/" <> settingsOwner <> "/" <> settingsRepo <> "/issues"

    licenseName, libModuleName :: Text
    licenseName   = show settingsLicenseName
    libModuleName = packageToModule settingsRepo

    testedGhcs :: Text
    testedGhcs = intercalateMap ", " (mappend "GHC == " . showGhcVer) settingsTestedVersions

    sourceRepository :: Text
    sourceRepository =
        [text|
        source-repository head
          type:                git
          location:            https://github.com/${settingsOwner}/${settingsRepo}.git
        $endLine
        |]

    libraryStanza :: Text
    libraryStanza =
        [text|
        library
          hs-source-dirs:      src
          exposed-modules:     $libModuleName
                               $preludeMod

          build-depends:       $settingsBase
                             $commaPreludeLibrary

          ghc-options:         -Wall
                               $ghcOptions

          default-language:    Haskell2010
          $defaultExtensions
        $endLine
        |]

    executableStanza :: Text -> Text
    executableStanza commaRepo =
        [text|
        executable $settingsRepo
          hs-source-dirs:      app
          main-is:             Main.hs

          build-depends:       $settingsBase
                             $commaRepo
                             $commaPreludeLibrary

          ghc-options:         -Wall
                               -threaded
                               -rtsopts
                               -with-rtsopts=-N
                               $ghcOptions

          default-language:    Haskell2010
          $defaultExtensions
        $endLine
        |]

    testSuiteStanza :: Text -> Text
    testSuiteStanza commaRepo =
        [text|
        test-suite ${settingsRepo}-test
          type:                exitcode-stdio-1.0
          hs-source-dirs:      test
          main-is:             Spec.hs

          build-depends:       $settingsBase
                             $commaRepo
                             $commaPreludeLibrary

          ghc-options:         -Wall
                               -threaded
                               -rtsopts
                               -with-rtsopts=-N
                               $ghcOptions

          default-language:    Haskell2010
          $defaultExtensions
        $endLine
        |]

    benchmarkStanza :: Text -> Text
    benchmarkStanza commaRepo =
        [text|
        benchmark ${settingsRepo}-benchmark
          type:                exitcode-stdio-1.0
          hs-source-dirs:      benchmark
          main-is:             Main.hs

          build-depends:       $settingsBase
                             , gauge
                             $commaRepo
                             $commaPreludeLibrary

          ghc-options:         -Wall
                               -threaded
                               -rtsopts
                               -with-rtsopts=-N
                               $ghcOptions

          default-language:    Haskell2010
          $defaultExtensions
        $endLine
        |]


    preludeMod, commaPreludeLibrary :: Text
    (preludeMod, commaPreludeLibrary) = case settingsPrelude of
        Nothing                -> ("", "")
        Just CustomPrelude{..} -> ("Prelude", ", " <> cpPackage)

    defaultExtensions :: Text
    defaultExtensions = case settingsExtensions of
        [] -> ""
        xs -> "default-extensions:  " <> T.intercalate "\n                     " xs

    ghcOptions :: Text
    ghcOptions = case settingsWarnings of
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
        =  memptyIfFalse (settingsTestedVersions `hasLeast` Ghc801)
            "-Wredundant-constraints\n"
        <> memptyIfFalse (settingsTestedVersions `hasLeast` Ghc822)
            "-fhide-source-paths\n"
        <> memptyIfFalse (settingsTestedVersions `hasLeast` Ghc843)
            [text|
            -Wmissing-export-lists
            -Wpartial-fields
            |]
      where
        hasLeast list el = all (>= el) list
