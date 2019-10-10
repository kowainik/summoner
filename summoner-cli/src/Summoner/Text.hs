module Summoner.Text
       ( packageToModule
       , intercalateMap
       , headToUpper
       , tconcatMap
       , alignTable
       ) where

import Data.Semigroup (Max (Max), getMax)

import qualified Data.Char as C
import qualified Data.Text as T

-- | Creates module name from the name of the package
-- Ex: @my-lovely-project@ — @MyLovelyProject@
packageToModule :: Text -> Text
packageToModule = tconcatMap headToUpper . T.splitOn "-"

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
      maxLengths :: (Max Int, Max Int, Max Int)
      maxLengths = getMaxLengths metas 

formatTriple :: (Max Int, Max Int, Max Int) -> (Text, Text, Text) -> Text
formatTriple (lenA, lenB, lenC) (a, b, c) = 
    padWord lenA a <> " " <> padWord lenB b <> " " <> padWord lenC c

padWord :: Max Int -> Text -> Text
padWord maxWordLength word = word <> T.replicate (getMax maxWordLength - T.length word) " "

getMaxLengths :: [(Text, Text, Text)] -> (Max Int, Max Int, Max Int)
getMaxLengths = foldMap (mapTriple (Max . T.length))

mapTriple :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTriple fn (a, b, c) = (fn a, fn b, fn c)