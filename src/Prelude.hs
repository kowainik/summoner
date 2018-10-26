-- | Uses @relude@ as a default prelude.

module Prelude
       ( module Relude
       , module Relude.Extra

       , endLine
       , memptyIfFalse
       , toListMap
       ) where

import Relude
import Relude.Extra.Enum as Relude.Extra (inverseMap, universe)

endLine :: Text
endLine = "\n"

memptyIfFalse :: Monoid m => Bool -> m -> m
memptyIfFalse p val = if p then val else mempty

toListMap :: Foldable f => (a -> b) -> f a -> [b]
toListMap f = map f . toList
