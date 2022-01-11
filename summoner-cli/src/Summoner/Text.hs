{- |
Module                  : Summoner.Text
Copyright               : (c) 2017-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Various helpful functions to work with 'Text'
-}

module Summoner.Text
       ( packageToModule
       , packageNameValid
       , moduleNameValid
       , intercalateMap
       , headToUpper
       , quote
       ) where

import qualified Data.Char as C
import qualified Data.Text as T


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

{- | Converts every element of list into 'Text' and then joins every element
into single 'Text' like 'T.intercalate'.
-}
intercalateMap :: Text -> (a -> Text) -> [a] -> Text
intercalateMap between showT = T.intercalate between . map showT

headToUpper :: Text -> Text
headToUpper t = case T.uncons t of
    Nothing      -> ""
    Just (x, xs) -> T.cons (C.toUpper x) xs

-- | Convert every element of a list into text, and squash the results
tconcatMap :: (a -> Text) -> [a] -> Text
tconcatMap f = T.concat . map f

-- | Wrap the given text into quotes.
quote :: Text -> Text
quote t = "\"" <> t <> "\""
