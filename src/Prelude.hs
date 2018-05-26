-- | Uses [universum](http://hackage.haskell.org/package/universum) as default prelude.

module Prelude
       ( module Universum
       , intercalateMap
       ) where

import Universum hiding (Key)

import qualified Data.Text as T

-- | Converts every element of list into 'Text' and then joins every element
-- into single 'Text' like 'T.intercalate'.
intercalateMap :: Text -> (a -> Text) -> [a] -> Text
intercalateMap between showT = T.intercalate between . map showT
-- TODO: maybe Prelude is not the best place for such function?...
