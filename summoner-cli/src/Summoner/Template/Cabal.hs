{-# LANGUAGE QuasiQuotes #-}

{- |
Copyright: (c) 2017-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

@.cabal@ file template.
-}

module Summoner.Template.Cabal
       ( cabalFile
       ) where

import NeatInterpolation (text)

import Summoner.CustomPrelude (CustomPrelude (..))
import Summoner.Default (defaultCabal)
import Summoner.GhcVer (GhcVer (..), cabalBaseVersions, showGhcVer)
import Summoner.License (LicenseName (..), cabalLicense)
import Summoner.Settings (Settings (..))
import Summoner.Template.Mempty (memptyIfFalse)
import Summoner.Text (endLine, intercalateMap, packageToModule)
import Summoner.Tree (TreeFs (..))

import qualified Data.Text as T


-- | Creates a `.cabal` file from the given 'Settings'.
cabalFile :: Settings -> TreeFs
cabalFile Settings{..} = File (toString settingsRepo ++ ".cabal") cabalFileContent
  where
    cabalFileContent :: Text
    cabalFileContent = T.concat
        [ cabalHeader
        , memptyIfFalse settingsGitHub sourceRepository
        , commonStanza
        , memptyIfFalse settingsIsLib libraryStanza
        , memptyIfFalse settingsIsExe executableStanza
        , memptyIfFalse settingsTest  testSuiteStanza
        , memptyIfFalse settingsBench $ benchmarkStanza $ memptyIfFalse settingsIsLib $ ", " <> settingsRepo
        ]

    -- TODO: do something to not have empty lines
    cabalHeader :: Text
    cabalHeader = unlines $
        [ "cabal-version:       " <> defaultCabal
        , "name:                " <> settingsRepo
        , "version:             0.0.0.0"
        , "synopsis:            " <> settingsDescription
        , "description:         " <> settingsDescription ] ++
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
        , "                     CHANGELOG.md"
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
        ("\n" <> T.replicate 21 " ")
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


    commonStanza :: Text
    commonStanza =
        [text|
        $endLine
        common common-options
          build-depends:       $settingsBaseType $baseBounds
                             $commaPreludeLibrary

          ghc-options:         -Wall
                               $ghcOptions

          default-language:    Haskell2010
    |] <> defaultExtensions

    baseBounds :: Text
    baseBounds = cabalBaseVersions settingsTestedVersions

    libraryStanza :: Text
    libraryStanza =
        [text|
        $endLine
        library
          import:              common-options
          hs-source-dirs:      src
          exposed-modules:     $libModuleName
                               $preludeMod
        |]

    executableStanza :: Text
    executableStanza =
        [text|
        $endLine
        executable $settingsRepo
          import:              common-options
          hs-source-dirs:      app
          main-is:             Main.hs
          $buildDepends
          $rtsOptions
        |]

    testSuiteStanza :: Text
    testSuiteStanza =
        [text|
        $endLine
        test-suite ${settingsRepo}-test
          import:              common-options
          type:                exitcode-stdio-1.0
          hs-source-dirs:      test
          main-is:             Spec.hs
          $buildDepends
          $rtsOptions
        |]

    benchmarkStanza :: Text -> Text
    benchmarkStanza commaRepo =
        [text|
        $endLine
        benchmark ${settingsRepo}-benchmark
          import:              common-options
          type:                exitcode-stdio-1.0
          hs-source-dirs:      benchmark
          main-is:             Main.hs
          build-depends:       gauge
                             $commaRepo
          $rtsOptions
          |]

    -- | @build-depends@ for the repo, only if the library is on.
    buildDepends :: Text
    buildDepends =
        if settingsIsLib
        then "build-depends:       " <> settingsRepo
        else ""

    rtsOptions :: Text
    rtsOptions =
        [text|
        ghc-options:         -threaded
                             -rtsopts
                             -with-rtsopts=-N
        |]

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
        <> memptyIfFalse (settingsTestedVersions `hasLeast` Ghc882)
            "-Wmissing-deriving-strategies\n"
      where
        hasLeast :: [GhcVer] -> GhcVer -> Bool
        hasLeast list el = all (>= el) list
