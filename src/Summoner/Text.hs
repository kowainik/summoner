module Summoner.Text
       ( packageToModule
       , intercalateMap
       ) where

import qualified Data.Text as T

-- | Creates module name from the name of the package
-- Ex: @my-lovely-project@ — @MyLovelyProject@
packageToModule :: Text -> Text
packageToModule = T.concat . map T.toTitle . T.splitOn "-"

-- | Converts every element of list into 'Text' and then joins every element
-- into single 'Text' like 'T.intercalate'.
intercalateMap :: Text -> (a -> Text) -> [a] -> Text
intercalateMap between showT = T.intercalate between . map showT
