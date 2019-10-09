module Summoner.Text
       ( packageToModule
       , intercalateMap
       , headToUpper
       , tconcatMap
       , alignTable
       ) where

import Data.List (maximum)

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
alignTable :: [[Text]] -> [Text]
alignTable ghcOutputs = map (T.intercalate " ") $
    transpose $ zipWith padRow (transpose ghcOutputs) maxWordLengths 
    where 
      maxWordLengths :: [Int]
      maxWordLengths = getMaxLengths ghcOutputs 

padRow :: [Text] -> Int -> [Text] 
padRow row maxWordLength = map (padWord maxWordLength) row

padWord :: Int -> Text -> Text
padWord maxWordLength word = word <> T.replicate (maxWordLength - T.length word) " "

getMaxLengths :: [[Text]] -> [Int]
getMaxLengths rows = map (maximum . map T.length) (transpose rows)