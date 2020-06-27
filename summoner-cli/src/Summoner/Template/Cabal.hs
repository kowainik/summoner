{-# LANGUAGE QuasiQuotes #-}

{- |
Module                  : Summoner.Template.Cabal
Copyright               : (c) 2017-2020 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@.cabal@ file template.
-}

module Summoner.Template.Cabal
       ( cabalFile
       ) where

import NeatInterpolation (text)

import Summoner.CustomPrelude (CustomPrelude (..))
import Summoner.Default (defaultCabal)
import Summoner.GhcVer (cabalBaseVersions, showGhcVer)
import Summoner.License (LicenseName (..))
import Summoner.Settings (Settings (..))
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
        [ "license-file:        LICENSE" | settingsLicenseName /= NONE] ++
        [ "author:              " <> settingsFullName
        , "maintainer:          " <> settingsFullName <> " <" <> settingsEmail <> ">"
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
    licenseName   = show settingsLicenseName
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
          build-depends:       base $baseBounds
          $customPrelude
          $ghcOptions

          default-language:    Haskell2010
    |] <> defaultExtensions

    baseBounds :: Text
    baseBounds = cabalBaseVersions settingsTestedVersions

    ghcOptions :: Text
    ghcOptions = case settingsGhcOptions of
        [] -> defaultGhcOptions
        x:xs ->
            let customGhcOptions = T.intercalate "\n" $ x : map (T.replicate 21 " " <>) xs in
            [text|
            ghc-options:         $customGhcOptions
            |]

    defaultGhcOptions :: Text
    defaultGhcOptions =
        [text|
        ghc-options:         -Wall
                             -Wcompat
                             -Widentities
                             -Wincomplete-uni-patterns
                             -Wincomplete-record-updates
        if impl(ghc >= 8.0)
          ghc-options:       -Wredundant-constraints
        if impl(ghc >= 8.2)
          ghc-options:       -fhide-source-paths
        if impl(ghc >= 8.4)
          ghc-options:       -Wmissing-export-lists
                             -Wpartial-fields
        if impl(ghc >= 8.8)
          ghc-options:       -Wmissing-deriving-strategies
        |]

    libraryStanza :: Text
    libraryStanza =
        [text|
        $endLine
        library
          import:              common-options
          hs-source-dirs:      src
          exposed-modules:     $libModuleName
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

    customPrelude :: Text
    customPrelude = case settingsPrelude of
        Nothing -> ""
        Just CustomPrelude{..} ->
            "                   , " <> cpPackage <> "\n" <>
            [text|
            $endLine
            mixins:              base hiding (Prelude)
                               , $cpPackage ($cpModule as Prelude)
            $endLine
            |]

    defaultExtensions :: Text
    defaultExtensions = case settingsExtensions of
        [] -> ""
        xs -> "  default-extensions:  "
           <> T.intercalate "\n                       " xs
           <> "\n"
