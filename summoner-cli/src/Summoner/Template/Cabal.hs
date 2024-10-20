{- |
Module                  : Summoner.Template.Cabal
Copyright               : (c) 2017-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@.cabal@ file template.
-}

module Summoner.Template.Cabal
       ( cabalFile
       ) where

import Colourista (indent)

import Summoner.CustomPrelude (CustomPrelude (..))
import Summoner.Default (defaultCabal)
import Summoner.GhcVer (cabalBaseVersions, showGhcVer)
import Summoner.License (LicenseName (..))
import Summoner.Settings (Settings (..))
import Summoner.Text (intercalateMap, packageToModule)
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
        , memptyIfFalse settingsBench benchmarkStanza
        ]

    cabalHeader :: Text
    cabalHeader = unlines $
        [ "cabal-version:       " <> defaultCabal
        , "name:                " <> settingsRepo
        , "version:             0.0.0.0"
        , "synopsis:            " <> settingsDescription
        , "description:"
        ] <> fullDescription <>
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

    fullDescription :: [Text]
    fullDescription =
        [ "    " <> addDot settingsDescription
        , "    See " <> readme <> " for more details."
        ]
      where
        readme :: Text
        readme = if settingsGitHub
           then "[README.md](" <> githubUrl <> "#" <> settingsRepo <> ")"
           else "README.md"

        addDot :: Text -> Text
        addDot "" = ""
        addDot txt = case T.last txt of
            '.'        -> txt
            _otherChar -> txt <> "."

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
    sourceRepository = unlines
        [ ""
        , "source-repository head"
        , "  type:                git"
        , "  location:            " <> githubUrl <> ".git"
        ]

    commonStanza :: Text
    commonStanza = unlines $
        [ ""
        , "common common-options"
        , "  build-depends:       base " <> baseBounds
        ]
        <> customPrelude
        <> ghcOptions
        <>
        ( ""
        : "  default-language:    Haskell2010"
        : defaultExtensions
        )

    baseBounds :: Text
    baseBounds = cabalBaseVersions settingsTestedVersions

    ghcOptions :: [Text]
    ghcOptions = "" : case settingsGhcOptions of
        []   -> defaultGhcOptions
        x:xs -> "  ghc-options:         " <> x : map (indent 23 <>) xs

    defaultGhcOptions :: [Text]
    defaultGhcOptions =
        [ "  ghc-options:         -Wall"
        , "                       -Wcompat"
        , "                       -Widentities"
        , "                       -Wincomplete-uni-patterns"
        , "                       -Wincomplete-record-updates"
        , "                       -Wredundant-constraints"
        , "                       -Wnoncanonical-monad-instances"
        , "  if impl(ghc >= 8.2)"
        , "    ghc-options:       -fhide-source-paths"
        , "  if impl(ghc >= 8.4)"
        , "    ghc-options:       -Wmissing-export-lists"
        , "                       -Wpartial-fields"
        , "  if impl(ghc >= 8.8)"
        , "    ghc-options:       -Wmissing-deriving-strategies"
        , "                       -fwrite-ide-info"
        , "                       -hiedir=.hie"
        , "  if impl(ghc >= 8.10)"
        , "    ghc-options:       -Wunused-packages"
        , "  if impl(ghc >= 9.0)"
        , "    ghc-options:       -Winvalid-haddock"
        , "  if impl(ghc >= 9.2)"
        , "    ghc-options:       -Wredundant-bang-patterns"
        , "                       -Woperator-whitespace"
        , "  if impl(ghc >= 9.4  && < 9.10)"
        , "    ghc-options:       -Wforall-identifier"
        , "  if impl(ghc >= 9.4)"
        , "    ghc-options:       -Wredundant-strictness-flags"
        , "  if impl(ghc >= 9.8)"
        , "    ghc-options:       -Wterm-variable-capture"
        , "                       -Winconsistent-flags"
        ]

    libraryStanza :: Text
    libraryStanza = unlines
        [ ""
        , "library"
        , "  import:              common-options"
        , "  hs-source-dirs:      src"
        , "  exposed-modules:     " <> libModuleName
        ]

    executableStanza :: Text
    executableStanza = unlines $
        [ ""
        , "executable " <> settingsRepo
        , "  import:              common-options"
        , "  hs-source-dirs:      app"
        , "  main-is:             Main.hs"
        ]
        <> buildDepends
        <> rtsOptions

    testSuiteStanza :: Text
    testSuiteStanza = unlines $
        [ ""
        , "test-suite " <> settingsRepo <> "-test"
        , "  import:              common-options"
        , "  type:                exitcode-stdio-1.0"
        , "  hs-source-dirs:      test"
        , "  main-is:             Spec.hs"
        ]
        <> buildDepends
        <> rtsOptions

    benchmarkStanza :: Text
    benchmarkStanza = unlines $
        [ ""
        , "benchmark " <> settingsRepo <> "-benchmark"
        , "  import:              common-options"
        , "  type:                exitcode-stdio-1.0"
        , "  hs-source-dirs:      benchmark"
        , "  main-is:             Main.hs"
        ]
        <> buildDepends
        <> rtsOptions

    -- | @build-depends@ for the repo, only if the library is on.
    buildDepends :: [Text]
    buildDepends = memptyIfFalse settingsIsLib
        ["  build-depends:       " <> settingsRepo]

    rtsOptions :: [Text]
    rtsOptions =
        [ "  ghc-options:         -threaded"
        , "                       -rtsopts"
        , "                       -with-rtsopts=-N"
        ]

    customPrelude :: [Text]
    customPrelude = case settingsPrelude of
        Nothing -> []
        Just CustomPrelude{..} ->
            [ "                     , " <> cpPackage
            , ""
            , "  mixins:              base hiding (Prelude)"
            , "                     , " <> cpPackage <> " (" <> cpModule <> " as Prelude)"
            ]

    defaultExtensions :: [Text]
    defaultExtensions = case settingsExtensions of
        [] -> []
        x:xs ->
             "  default-extensions:  " <> x
           : map (indent 23 <>) xs
