{-# LANGUAGE QuasiQuotes #-}

{- |
Copyright: (c) 2017-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Templates for generated Haskell source code files.
-}

module Summoner.Template.Haskell
       ( haskellFiles
       ) where

import NeatInterpolation (text)

import Summoner.CustomPrelude (CustomPrelude (..))
import Summoner.Settings (Settings (..))
import Summoner.Text (packageToModule)
import Summoner.Tree (TreeFs (..))


haskellFiles :: Settings -> [TreeFs]
haskellFiles Settings{..} = concat
    [ [ Dir "src"     $ libFile : preludeFile   | settingsIsLib ]
    , [ Dir "app"       [exeFile]               | settingsIsExe ]
    , [ Dir "test"      [testFile]              | settingsTest  ]
    , [ Dir "benchmark" [benchmarkFile]         | settingsBench ]
    ] ++ maybeToList (File ".stylish-haskell.yaml" <$> settingsStylish)
  where
    libFile :: TreeFs
    libFile = File (toString libModuleName <> ".hs")
        [text|
        {- |
        Copyright: (c) $settingsYear $settingsFullName
        SPDX-License-Identifier: $licenseName
        Maintainer: $settingsFullName <$settingsEmail>

        $settingsDescription
        -}

        module $libModuleName
               ( someFunc
               ) where


        someFunc :: IO ()
        someFunc = putStrLn ("someFunc" :: String)
        |]

    libModuleName :: Text
    libModuleName = packageToModule settingsRepo

    licenseName :: Text
    licenseName = show settingsLicenseName

    preludeFile :: [TreeFs]
    preludeFile = maybeToList $
        settingsPrelude <&> \CustomPrelude{..} -> File "Prelude.hs"
            [text|
            -- | Uses [$cpPackage](https://hackage.haskell.org/package/${cpPackage}) as default Prelude.

            module Prelude
                   ( module $cpModule
                   ) where

            import $cpModule
            |]

    exeFile :: TreeFs
    exeFile = File "Main.hs" $ if settingsIsLib then createExe else createOnlyExe

    createOnlyExe :: Text
    createOnlyExe =
        [text|
        module Main (main) where


        main :: IO ()
        main = putStrLn ("Hello, world!" :: String)
        |]

    createExe :: Text
    createExe =
        [text|
        module Main (main) where

        import $libModuleName (someFunc)


        main :: IO ()
        main = someFunc
        |]

    testFile :: TreeFs
    testFile = File "Spec.hs"
        [text|
        module Main (main) where


        main :: IO ()
        main = putStrLn ("Test suite is not implemented" :: String)
        |]

    benchmarkFile :: TreeFs
    benchmarkFile = File "Main.hs"
        [text|
        module Main (main) where

        import Gauge.Main


        main :: IO ()
        main = defaultMain [bench "const" (whnf const ())]
        |]
