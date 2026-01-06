{- |
Module                  : Summoner.Template.Haskell
Copyright               : (c) 2017-2026 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Templates for generated Haskell source code files.
-}

module Summoner.Template.Haskell
       ( haskellFiles
       ) where

import Summoner.Settings (Settings (..))
import Summoner.Text (packageToModule, quote)
import Summoner.Tree (TreeFs (..))


haskellFiles :: Settings -> [TreeFs]
haskellFiles Settings{..} = concat
    [ [ Dir "src"       [libFile]       | settingsIsLib ]
    , [ Dir "app"       [exeFile]       | settingsIsExe ]
    , [ Dir "test"      [testFile]      | settingsTest  ]
    , [ Dir "benchmark" [benchmarkFile] | settingsBench ]
    ]
  where
    libFile :: TreeFs
    libFile = File (toString libModuleName <> ".hs") $ unlines
        [ "{- |"
        , "Copyright: (c) " <> settingsYear <> " " <> settingsFullName
        , "SPDX-License-Identifier: " <> licenseName
        , "Maintainer: " <> settingsFullName <> " <" <> settingsEmail <> ">"
        , ""
        , settingsDescription
        , "-}"
        , ""
        , "module " <> libModuleName
        , "    ( projectName"
        , "    ) where"
        , ""
        , ""
        , "projectName :: String"
        , "projectName = " <> quote settingsRepo
        ]

    libModuleName :: Text
    libModuleName = packageToModule settingsRepo

    licenseName :: Text
    licenseName = show settingsLicenseName

    exeFile :: TreeFs
    exeFile = File "Main.hs" $ stanza "Executable"

    testFile :: TreeFs
    testFile = File "Spec.hs" $ stanza "Tests"

    benchmarkFile :: TreeFs
    benchmarkFile = File "Main.hs" $ stanza "Benchmarks"

    stanza :: Text -> Text
    stanza st = unlines $
          "module Main (main) where"
        : memptyIfFalse settingsIsLib
            [ ""
            , "import " <> libModuleName <> " (projectName)"
            ]
        <>
        [ ""
        , ""
        , "main :: IO ()"
        , mainContent
        ]
      where
        mainContent :: Text
        mainContent = if settingsIsLib
            then "main = putStrLn (" <> quote (st <> " for ") <> " ++ projectName)"
            else "main = putStrLn (" <> quote (st <> " not implemented") <> " :: String)"
