{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ViewPatterns     #-}

module Summoner.Template.Cabal
       ( cabalFile
       ) where

import Named ((:!), arg)
import NeatInterpolation (text)

import Summoner.GhcVer (GhcVer (..), showGhcVer)
import Summoner.Settings (CustomPrelude (..))
import Summoner.Text (intercalateMap)
import Summoner.Tree (TreeFs (..))

import qualified Data.Text as T


cabalFile
    :: "repo"           :! Text
    -> "owner"          :! Text
    -> "description"    :! Text
    -> "license"        :! Text
    -> "fullName"       :! Text
    -> "email"          :! Text
    -> "year"           :! Text
    -> "categories"     :! Text
    -> "base"           :! Text
    -> "libModuleName"  :! Text
    -> "github"         :! Bool
    -> "isLib"          :! Bool
    -> "isExe"          :! Bool
    -> "test"           :! Bool
    -> "bench"          :! Bool
    -> "warnings"       :! [Text]
    -> "extensions"     :! [Text]
    -> "testedVersions" :! [GhcVer]
    -> "prelude"        :! Maybe CustomPrelude
    -> TreeFs
cabalFile
    (arg #repo           -> repo)
    (arg #owner          -> owner)
    (arg #description    -> description)
    (arg #license        -> license)
    (arg #fullName       -> fullName)
    (arg #email          -> email)
    (arg #year           -> year)
    (arg #categories     -> categories)
    (arg #base           -> base)
    (arg #libModuleName  -> libModuleName)
    (arg #github         -> github)
    (arg #isLib          -> isLib)
    (arg #isExe          -> isExe)
    (arg #test           -> test)
    (arg #bench          -> bench)
    (arg #warnings       -> warnings)
    (arg #extensions     -> extensions)
    (arg #testedVersions -> testedVersions)
    (arg #prelude        -> prelude)
    = File (toString repo ++ ".cabal") cabalFileContent
  where
    cabalFileContent :: Text
    cabalFileContent = mconcat
        [ cabalHeader
        , memptyIfFalse github sourceRepository
        , memptyIfFalse isLib   libraryStanza
        , memptyIfFalse isExe $ executableStanza $ memptyIfFalse isLib $ ", " <> repo
        , memptyIfFalse test  $ testSuiteStanza  $ memptyIfFalse isLib $ ", " <> repo
        , memptyIfFalse bench $ benchmarkStanza  $ memptyIfFalse isLib $ ", " <> repo
        ]

    -- TODO: do something to not have empty lines
    cabalHeader :: Text
    cabalHeader =
        [text|
        cabal-version:       2.0
        name:                $repo
        version:             0.0.0
        synopsis:            $description
        description:         $description
        $githubHomepage
        $githubBugReports
        license:             $license
        license-file:        LICENSE
        author:              $fullName
        maintainer:          $email
        copyright:           $year $fullName
        category:            $categories
        build-type:          Simple
        extra-doc-files:     README.md
                           , CHANGELOG.md
        tested-with:         $testedGhcs
        $endLine
        |]

    githubHomepage, githubBugReports :: Text
    githubHomepage   = memptyIfFalse github $ "homepage:            https://github.com/" <> owner <> "/" <> repo
    githubBugReports = memptyIfFalse github $ "bug-reports:         https://github.com/" <> owner <> "/" <> repo <> "/issues"

    testedGhcs :: Text
    testedGhcs = intercalateMap ", " (mappend "GHC == " . showGhcVer) testedVersions

    sourceRepository :: Text
    sourceRepository =
        [text|
        source-repository head
          type:                git
          location:            https://github.com/${owner}/${repo}.git
        $endLine
        |]

    libraryStanza :: Text
    libraryStanza =
        [text|
        library
          hs-source-dirs:      src
          exposed-modules:     $libModuleName
                               $preludeMod

          build-depends:       $base
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
        executable $repo
          hs-source-dirs:      app
          main-is:             Main.hs

          build-depends:       $base
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
        test-suite ${repo}-test
          type:                exitcode-stdio-1.0
          hs-source-dirs:      test
          main-is:             Spec.hs

          build-depends:       $base
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
        benchmark ${repo}-benchmark
          type:                exitcode-stdio-1.0
          hs-source-dirs:      benchmark
          main-is:             Main.hs

          build-depends:       $base
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
    (preludeMod, commaPreludeLibrary) = case prelude of
        Nothing                -> ("", "")
        Just CustomPrelude{..} -> ("Prelude", ", " <> cpPackage)

    defaultExtensions :: Text
    defaultExtensions = case extensions of
        [] -> ""
        xs -> "default-extensions:  " <> T.intercalate "\n                     " xs

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
