{- |
Module                  : Summoner.Template.Haskell
Copyright               : (c) 2017-2020 Kowainik
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
        , "module " <>libModuleName
        , "       ( someFunc"
        , "       ) where"
        , ""
        , ""
        , "someFunc :: IO ()"
        , "someFunc = putStrLn (" <> quote "someFunc" <> " :: String)"
        ]

    libModuleName :: Text
    libModuleName = packageToModule settingsRepo

    licenseName :: Text
    licenseName = show settingsLicenseName

    exeFile :: TreeFs
    exeFile = File "Main.hs" $ if settingsIsLib then createExe else createOnlyExe

    createOnlyExe :: Text
    createOnlyExe = unlines
        [ "module Main (main) where"
        , ""
        , ""
        , "main :: IO ()"
        , "main = putStrLn (" <> quote "Hello, world!" <> " :: String)"
        ]

    createExe :: Text
    createExe = unlines
        [ "module Main (main) where"
        , ""
        , "import " <> libModuleName <> " (someFunc)"
        , ""
        , ""
        , "main :: IO ()"
        , "main = someFunc"
        ]

    testFile :: TreeFs
    testFile = File "Spec.hs" $ unlines
        [ "module Main (main) where"
        , ""
        , ""
        , "main :: IO ()"
        , "main = putStrLn (" <> quote "Test suite is not implemented" <> " :: String)"
        ]

    benchmarkFile :: TreeFs
    benchmarkFile = File "Main.hs" $ unlines
        [ "module Main (main) where"
        , ""
        , "import Gauge.Main"
        , ""
        , ""
        , "main :: IO ()"
        , "main = defaultMain [bench " <> quote "const" <> " (whnf const ())]"
        ]
