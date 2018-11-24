-- | Uses [relude](https://hackage.haskell.org/package/relude) as default Prelude.

module Prelude
       ( module Relude
       , module Relude.Extra
       ) where

import Relude
import Relude.Extra.Enum as Relude.Extra (universe)
import Relude.Extra.Validation as Relude.Extra
