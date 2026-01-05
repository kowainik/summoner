{-# OPTIONS_GHC -fno-warn-orphans #-}

{- | Golden tests for @summoner@.

In the @test/golden@ folder we now have two projects that are created by
@summoner@.

* @fullProject@ – the project that has all options enabled.
* @smallProject@ – the project that has limited options enabled.

The purpose of this tests is to check that with the corresponding 'Settings'
the tool creates the expected project.

-}

module Test.Golden
       ( goldenSpec
       ) where

import Data.TreeDiff (ToExpr, ansiWlEditExprCompact, ediff)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeFileName, (</>))
import Test.Hspec (Spec, describe, expectationFailure, it)

import Summoner (Settings)
import Summoner.Golden (cabalFull, cabalMinimal, fullBatteries, stackFull)
import Summoner.Template (createProjectTemplate)
import Summoner.Tree (TreeFs (..))

import qualified Data.Text as T


goldenSpec :: Spec
goldenSpec = describe "golden tests" $ do
    it "correctly scaffolds the 'cabal-minimal' project" $
        checkProject "examples/cabal-minimal" cabalMinimal
    it "correctly scaffolds the 'cabal-full' project" $
        checkProject "examples/cabal-full" cabalFull
    it "correctly scaffolds the 'stack-full' project" $
        checkProject "examples/stack-full" stackFull
    it "correctly scaffolds the 'full-batteries' project" $
        checkProject "examples/full-batteries" fullBatteries
  where
    checkProject :: FilePath -> Settings -> IO ()
    checkProject path settings = do
        goldenFs  <- sortTree <$> readTreeFs path
        let testFs = sortTree $ createProjectTemplate settings
        when (goldenFs /= testFs) $ do
            putTextLn $ show $ ansiWlEditExprCompact $ ediff goldenFs testFs
            expectationFailure "Golden and scaffolded project don't match"

readTreeFs :: FilePath -> IO TreeFs
readTreeFs filePath = doesDirectoryExist filePath >>= \case
    True -> do
        dirs <- listDirectory filePath
        Dir (takeFileName filePath) <$> traverse (\dir -> readTreeFs $ filePath </> dir) dirs
    False -> do
        -- Normalize line endings (CRLF -> LF) for cross-platform compatibility
        content <- T.filter (/= '\r') . decodeUtf8 <$> readFileBS filePath
        pure $ File (takeFileName filePath) content

sortTree :: TreeFs -> TreeFs
sortTree = \case
    file@(File _ _) -> file
    Dir path fs     -> Dir path $ sort $ map sortTree fs

-- Orphan instances

instance ToExpr TreeFs
