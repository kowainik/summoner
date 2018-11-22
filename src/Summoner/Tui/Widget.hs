{- | This module contains useful helpers to work with 'Widget's
-}

module Summoner.Tui.Widget
       ( label
       , borderLabel
       , hArrange
       ) where

import Brick (Padding (Pad), Widget, hBox, padRight, str, (<+>))
import Brick.Widgets.Border (borderWithLabel)


-- | Adds label to the Form's field.
label :: String -> Widget n -> Widget n
label l = (<+>) (str l)

-- | Like 'borderWithLabel' but receives 'String'.
borderLabel :: String -> Widget n -> Widget n
borderLabel l = borderWithLabel (str l)

{- | Arranges 'Widget's horizontally.

Example:

@
 [x] Cabal   [ ] Stack
@
-}
hArrange :: [Widget n] -> Widget n
hArrange = hBox . updateHead (padRight (Pad 2))
  where
    updateHead :: (a -> a) -> [a] -> [a]
    updateHead _ []       = []
    updateHead f (a : as) = f a : as
