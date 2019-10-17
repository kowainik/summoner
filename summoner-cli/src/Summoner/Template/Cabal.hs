{-# LANGUAGE QuasiQuotes #-}

module Summoner.Template.Cabal
       ( cabalFile
       ) where

import NeatInterpolation (text)

import Summoner.Default (defaultCabal)
import Summoner.GhcVer (GhcVer (..), cabalBaseVersions, showGhcVer)
import Summoner.License (LicenseName (..), cabalLicense)
import Summoner.Settings (CustomPrelude (..), Settings (..))
import Summoner.Text (intercalateMap, packageToModule)
import Summoner.Tree (TreeFs (..))

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T


cabalFile :: Settings -> TreeFs
cabalFile Settings{..} = File (toString settingsRepo ++ ".cabal") cabalFileContent
  where
    cabalFileContent :: Text
    cabalFileContent = T.concat
        [ cabalHeader
        , memptyIfFalse settingsGitHub sourceRepository
        , memptyIfFalse settingsIsLib   libraryStanza
        , memptyIfFalse settingsIsExe $ executableStanza $ memptyIfFalse settingsIsLib $ ", " <> settingsRepo
        , memptyIfFalse settingsTest  $ testSuiteStanza  $ memptyIfFalse settingsIsLib $ ", " <> settingsRepo
        , memptyIfFalse settingsBench $ benchmarkStanza  $ memptyIfFalse settingsIsLib $ ", " <> settingsRepo
        ]

    settingsDescriptionLines :: NonEmpty Text
    settingsDescriptionLines =
      fromMaybe (settingsDescription NE.:| []) (nonEmpty (lines settingsDescription))

    -- TODO: do something to not have empty lines
    cabalHeader :: Text
    cabalHeader = unlines $
        [ "cabal-version:       " <> defaultCabal
        , "name:                " <> settingsRepo
        , "version:             0.0.0.0"
        , "synopsis:            " <> NE.head settingsDescriptionLines ] ++
        [ "                     " <> desc | desc <- NE.tail settingsDescriptionLines ] ++
        [ "description:         " <> NE.head settingsDescriptionLines ] ++
        [ "                     " <> desc | desc <- NE.tail settingsDescriptionLines ] ++
        [ "homepage:            " <> githubUrl        | settingsGitHub ] ++
        [ "bug-reports:         " <> githubBugReports | settingsGitHub ] ++
        ( "license:             " <> licenseName) :
        [ "license-file:        LICENSE" | settingsLicenseName /= None] ++
        [ "author:              " <> settingsFullName
        , "maintainer:          " <> settingsEmail
        , "copyright:           " <> settingsYear <> " " <> settingsFullName ] ++
        [ "category:            " <> settingsCategories | "" /= settingsCategories ] ++
        [ "build-type:          Simple"
        , "extra-doc-files:     README.md"
        , "                   , CHANGELOG.md"
        , "tested-with:         " <> testedGhcs
        ]

    githubUrl, githubBugReports :: Text
    githubUrl        = "https://github.com/" <> settingsOwner <> "/" <> settingsRepo
    githubBugReports = githubUrl <> "/issues"

    licenseName, libModuleName :: Text
    licenseName   = cabalLicense settingsLicenseName
    libModuleName = packageToModule settingsRepo

    testedGhcs :: Text
    testedGhcs = intercalateMap
        ("\n" <> T.replicate 19 " " <> ", ")
        (mappend "GHC == " . showGhcVer)
        settingsTestedVersions

    sourceRepository :: Text
    sourceRepository =
        [text|
        $endLine
        source-repository head
          type:                git
          location:            ${githubUrl}.git
        |]

    baseBounds :: Text
    baseBounds = cabalBaseVersions settingsTestedVersions

    libraryStanza :: Text
    libraryStanza =
        [text|
        $endLine
        library
          hs-source-dirs:      src
          exposed-modules:     $libModuleName
                               $preludeMod

          build-depends:       $settingsBaseType $baseBounds
                             $commaPreludeLibrary

          ghc-options:         -Wall
                               $ghcOptions

          default-language:    Haskell2010
        |] <> defaultExtensions

    executableStanza :: Text -> Text
    executableStanza commaRepo =
        [text|
        $endLine
        executable $settingsRepo
          hs-source-dirs:      app
          main-is:             Main.hs

          build-depends:       $settingsBaseType $baseBounds
                             $commaRepo
                             $commaPreludeLibrary

          ghc-options:         -Wall
                               -threaded
                               -rtsopts
                               -with-rtsopts=-N
                               $ghcOptions

          default-language:    Haskell2010
        |] <> defaultExtensions

    testSuiteStanza :: Text -> Text
    testSuiteStanza commaRepo =
        [text|
        $endLine
        test-suite ${settingsRepo}-test
          type:                exitcode-stdio-1.0
          hs-source-dirs:      test
          main-is:             Spec.hs

          build-depends:       $settingsBaseType $baseBounds
                             $commaRepo
                             $commaPreludeLibrary

          ghc-options:         -Wall
                               -threaded
                               -rtsopts
                               -with-rtsopts=-N
                               $ghcOptions

          default-language:    Haskell2010
        |] <> defaultExtensions

    benchmarkStanza :: Text -> Text
    benchmarkStanza commaRepo =
        [text|
        $endLine
        benchmark ${settingsRepo}-benchmark
          type:                exitcode-stdio-1.0
          hs-source-dirs:      benchmark
          main-is:             Main.hs

          build-depends:       $settingsBaseType $baseBounds
                             , gauge
                             $commaRepo
                             $commaPreludeLibrary

          ghc-options:         -Wall
                               -threaded
                               -rtsopts
                               -with-rtsopts=-N
                               $ghcOptions

          default-language:    Haskell2010
        |] <> defaultExtensions


    preludeMod, commaPreludeLibrary :: Text
    (preludeMod, commaPreludeLibrary) = case settingsPrelude of
        Nothing                -> ("", "")
        Just CustomPrelude{..} -> ("Prelude", ", " <> cpPackage)

    defaultExtensions :: Text
    defaultExtensions = case settingsExtensions of
        [] -> ""
        xs -> "  default-extensions:  "
           <> T.intercalate "\n                       " xs
           <> "\n"

    ghcOptions :: Text
    ghcOptions = case settingsGhcOptions of
        [] -> defaultGhcOptions
        xs -> T.intercalate "\n" xs

    defaultGhcOptions :: Text
    defaultGhcOptions =
        [text|
        -Wincomplete-uni-patterns
        -Wincomplete-record-updates
        -Wcompat
        -Widentities
        $versionGhcOptions
        |]

    versionGhcOptions :: Text
    versionGhcOptions
        =  memptyIfFalse (settingsTestedVersions `hasLeast` Ghc802)
            "-Wredundant-constraints\n"
        <> memptyIfFalse (settingsTestedVersions `hasLeast` Ghc822)
            "-fhide-source-paths\n"
        <> memptyIfFalse (settingsTestedVersions `hasLeast` Ghc844)
            [text|
            -Wmissing-export-lists
            -Wpartial-fields
            |]
      where
        hasLeast :: [GhcVer] -> GhcVer -> Bool
        hasLeast list el = all (>= el) list
