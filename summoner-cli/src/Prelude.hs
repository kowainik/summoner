-- | Uses @relude@ as a default prelude.

module Prelude
       ( module Relude
       , module Relude.Extra

       , endLine
       , memptyIfFalse
       ) where

import Relude
import Relude.Extra.Enum as Relude.Extra (inverseMap, universe)
import Relude.Extra.Validation as Relude.Extra

endLine :: Text
endLine = "\n"

memptyIfFalse :: Monoid m => Bool -> m -> m
memptyIfFalse p val = if p then val else mempty
