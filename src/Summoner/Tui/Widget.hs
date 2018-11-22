{- | This module contains useful helpers to work with 'Widget's
-}

module Summoner.Tui.Widget
       ( label
       , borderLabel
       , hArrange
       , listInBorder
       ) where

import Brick (Padding (Pad), Widget, padRight, (<+>))
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (hBox, hLimit, str, txtWrap, vBox, vLimit, withAttr)


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

{- | Shows list of elements inside 'borderWithLabel'.

__Example:__
@
┌────────────Some label──────────────────┐
│                                        │
│          > 8.2.2                       │
│          > 8.4.3                       │
│          > 8.4.4                       │
│                                        │
└────────────────────────────────────────┘
@

-}
listInBorder
    :: String  -- ^ Border label
    -> Int     -- ^ Horizontal limit
    -> Int     -- ^ Additional vertical limit
    -> [Text]  -- ^ List of text entries
    -> Widget n
listInBorder name limitH extraLimitV list = center
    $ hLimit limitH
    $ vLimit (length list + 4 + extraLimitV)
    $ borderLabel name
    $ center
    $ vBox
    $ map (withAttr "blue-fg" . txtWrap . ("➤ " <>)) list
