{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes      #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE ViewPatterns     #-}

module Summoner.Template.Haskell
       ( haskellFiles
       ) where

import Named ((:!), arg)
import NeatInterpolation (text)

import Summoner.Settings (CustomPrelude (..))
import Summoner.Tree (TreeFs (..))


haskellFiles
    :: "libModuleName" :! Text
    -> "isLib"         :! Bool
    -> "isExe"         :! Bool
    -> "test"          :! Bool
    -> "bench"         :! Bool
    -> "stylish"       :! Maybe Text
    -> "prelude"       :! Maybe CustomPrelude
    -> [TreeFs]
haskellFiles
    (arg #libModuleName -> libModuleName)
    (arg #isLib         -> isLib)
    (arg #isExe         -> isExe)
    (arg #test          -> test)
    (arg #bench         -> bench)
    (arg #stylish       -> stylish)
    (arg #prelude       -> prelude)
    = concat
    [ [ Dir "src"     $ libFile : preludeFile   | isLib ]
    , [ Dir "app"       [exeFile]               | isExe ]
    , [ Dir "test"      [testFile]              | test  ]
    , [ Dir "benchmark" [benchmarkFile]         | bench ]
    ] ++ maybe [] (\x -> [File ".stylish-haskell.yaml" x]) stylish
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

    preludeFile :: [TreeFs]
    preludeFile = case prelude of
        Nothing -> []
        Just CustomPrelude{..} -> one $ File "Prelude.hs"
            [text|
            -- | Uses [$cpPackage](https://hackage.haskell.org/package/${cpPackage}) as default Prelude.

            module Prelude
                   ( module $cpModule
                   ) where

            import $cpModule
            $endLine
            |]

    exeFile :: TreeFs
    exeFile = File "Main.hs" $ if isLib then createExe else createOnlyExe

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
