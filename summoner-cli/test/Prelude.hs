-- | Uses @relude@ as a default prelude.

module Prelude
       ( module Relude
       , module Relude.Extra
       ) where

import Relude
import Relude.Extra.Enum as Relude.Extra (inverseMap, universe)
