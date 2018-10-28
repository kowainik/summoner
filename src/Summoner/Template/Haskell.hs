{-# LANGUAGE QuasiQuotes #-}

module Summoner.Template.Haskell
       ( haskellFiles
       ) where

import NeatInterpolation (text)

import Summoner.Settings (CustomPrelude (..), Settings (..))
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
        module $libModuleName
               ( someFunc
               ) where

        someFunc :: IO ()
        someFunc = putStrLn ("someFunc" :: String)
        $endLine
        |]

    libModuleName :: Text
    libModuleName = packageToModule settingsRepo

    preludeFile :: [TreeFs]
    preludeFile = maybeToList $
        settingsPrelude <&> \CustomPrelude{..} -> File "Prelude.hs"
            [text|
            -- | Uses [$cpPackage](https://hackage.haskell.org/package/${cpPackage}) as default Prelude.

            module Prelude
                   ( module $cpModule
                   ) where

            import $cpModule
            $endLine
            |]

    exeFile :: TreeFs
    exeFile = File "Main.hs" $ if settingsIsLib then createExe else createOnlyExe

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

    testFile :: TreeFs
    testFile = File "Spec.hs"
        [text|
        main :: IO ()
        main = putStrLn ("Test suite not yet implemented" :: String)
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
