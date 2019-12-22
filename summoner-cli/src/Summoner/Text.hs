{- |
Copyright: (c) 2017-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Various helpful functions to work with 'Text'
-}

module Summoner.Text
       ( endLine
       , packageToModule
       , packageNameValid
       , moduleNameValid
       , intercalateMap
       , headToUpper
       , tconcatMap
       , alignTable
       ) where

import Data.Semigroup (Max (..))

import qualified Data.Char as C
import qualified Data.Text as T


-- | Endline symbol to use with @neat-interpolation@.
endLine :: Text
endLine = "\n"

-- | Creates module name from the name of the package
-- Ex: @my-lovely-project@ — @MyLovelyProject@
packageToModule :: Text -> Text
packageToModule = tconcatMap headToUpper . T.splitOn "-"

-- | Decides whether the given text is a valid package name. Spec is here:
-- https://www.haskell.org/cabal/users-guide/developing-packages.html#package-names-and-versions
packageNameValid :: Text -> Bool
packageNameValid = T.all (\c -> c == '-' || C.isAlphaNum c)

{- | Validate module name. It should be in the following formatTriple

@
Part1[.PartN]
@
-}
moduleNameValid :: Text -> Bool
moduleNameValid = all isValidFragment . T.split (== '.')
  where
    isValidFragment :: Text -> Bool
    isValidFragment s =
           s /= ""
        && T.all C.isAlphaNum s
        && C.isUpper (T.head s)

-- | Converts every element of list into 'Text' and then joins every element
-- into single 'Text' like 'T.intercalate'.
intercalateMap :: Text -> (a -> Text) -> [a] -> Text
intercalateMap between showT = T.intercalate between . map showT

headToUpper :: Text -> Text
headToUpper t = case T.uncons t of
    Nothing      -> ""
    Just (x, xs) -> T.cons (C.toUpper x) xs

-- | Convert every element of a list into text, and squash the results
tconcatMap :: (a -> Text) -> [a] -> Text
tconcatMap f = T.concat . map f

-- | Aligns a list of texts by their columns
alignTable :: [(Text, Text, Text)] -> [Text]
alignTable metas = map (formatTriple maxLengths) metas
  where
    maxLengths :: (Int, Int, Int)
    maxLengths = mapTriple getMax $ getMaxLengths metas

formatTriple :: (Int, Int, Int) -> (Text, Text, Text) -> Text
formatTriple (lenA, lenB, lenC) (a, b, c) =
    padRight lenA a <> "  " <> padRight lenB b <> "  " <> padRight lenC c

{- |
@padRight n t@ pads the text 't' with spaces on the right until it reaches length 'n'.
@
padRight 10 "hello"' ≡ "hello     "
padRight  3 "hello"' ≡ "hello"
@
-}
padRight :: Int -> Text -> Text
padRight n t = t <> T.replicate (n - T.length t) " "

getMaxLengths :: [(Text, Text, Text)] -> (Max Int, Max Int, Max Int)
getMaxLengths = foldMap (mapTriple (Max . T.length))

mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple fn (a, b, c) = (fn a, fn b, fn c)
