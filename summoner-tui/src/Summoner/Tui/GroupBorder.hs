{- |
Module                  : Summoner.Tui.GroupBorder
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

@Brick@ library helper functions to group checkbox elements inside the form.
This is not going to be the part of the library itself, so we will have it in
our own libraries. See relevant discussion under the corresponding issue:

* https://github.com/jtdaugherty/brick/issues/190
-}

module Summoner.Tui.GroupBorder
       ( groupBorder
       , (|>)
       ) where

import Brick (Edges (..), Padding (Max), Widget, padRight, vLimit, (<+>), (<=>))
import Brick.Forms (FormFieldState, (@@=))
import Brick.Widgets.Border (hBorder, hBorderWithLabel, joinableBorder, vBorder)

import Summoner.Tui.Widget (borderLabel, borderName)


-- | Create a pair of elements.
infix 4 |>
(|>) :: Int -> a -> (Int, a)
(|>) = (,)
{-# INLINE (|>) #-}


{- |
This function unites any amount of the form elements under the one group in
bourders with the given group name. Intended to be used for joining check-boxes
together, but any other elemens of the form will work the same way.

__Example:__

@
┌─────────────────Form───────────────────┐
│                                        │
│┌──────────────Accounts────────────────┐│
││[ ] user1                             ││
││[ ] User2                             ││
│└──────────────────────────────────────┘│
└────────────────────────────────────────┘
@

**Note:** on an empty list it doesn't create any group or border.
-}
groupBorder :: String -> [(Int, s -> FormFieldState s e n)] -> [s -> FormFieldState s e n]
groupBorder groupName  = \case
    []       -> []
    [x]      -> [groupAllBorders groupName x]
    (x:y:xs) -> let (mid, l) = (init $ y :| xs, last $ y :| xs) in
        groupBorderTop groupName x : map groupBorderMid mid ++ [groupBorderBottom l]

-- | Creates the top border with the group name.
groupBorderTop :: String -> (Int, s -> FormFieldState s e n) -> (s -> FormFieldState s e n)
groupBorderTop groupName (i, f) =
    ( vLimit i
    . ((tl <=> vBorder) <+>)
    . (<+> (tr <=> vBorder))
    . (hBorderWithLabel (borderName groupName) <=>)
    ) @@= f

-- | Creates the bottom border of the group.
groupBorderBottom :: (Int, s -> FormFieldState s e n) -> (s -> FormFieldState s e n)
groupBorderBottom (i, f) =
    ( vLimit i
    . ((vBorder <=> bl) <+>)
    . (<+> (vBorder <=> br))
    . (<=> hBorder)
    . padRight Max
    ) @@= f

-- | Creates the left and right borders for the middle elements of the group.
groupBorderMid :: (Int, s -> FormFieldState s e n) -> (s -> FormFieldState s e n)
groupBorderMid (i, f) =
    ( vLimit i
    . (vBorder <+>)
    . (<+> vBorder)
    . padRight Max
    ) @@= f

-- | Creates the border around the only one element.
groupAllBorders :: String -> (Int, s -> FormFieldState s e n) -> (s -> FormFieldState s e n)
groupAllBorders groupName (i, f) =
    ( vLimit i
    . borderLabel groupName
    . padRight Max
    ) @@= f


-- | Helpers for the correct border lines.
tl, tr, bl, br :: Widget n
tl = joinableBorder (Edges False True False True)
tr = joinableBorder (Edges False True True False)
bl = joinableBorder (Edges True False False True)
br = joinableBorder (Edges True False True False)
