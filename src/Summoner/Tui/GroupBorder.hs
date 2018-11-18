{- |
@Brick@ library helper functions to group checkbox elements inside the form.
This is not going to be the part of the library itself, so we will have it in
our own libraries. See relevant discussion under the corresponding issue:
* https://github.com/jtdaugherty/brick/issues/190
-}

module Summoner.Tui.GroupBorder
       ( groupBorder
       , (|>)
       ) where

import Brick (Edges (..), Padding (Max), Widget, padRight, str, vLimit, (<+>), (<=>))
import Brick.Forms (FormFieldState, (@@=))

import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as B
import qualified Brick.Widgets.Core as B


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
    . B.withBorderStyle B.unicodeBold
    . ((tl <=> B.vBorder) <+>)
    . (<+> (tr <=> B.vBorder))
    . (B.hBorderWithLabel (str groupName) <=>)
    ) @@= f

-- | Creates the bottom border of the group.
groupBorderBottom :: (Int, s -> FormFieldState s e n) -> (s -> FormFieldState s e n)
groupBorderBottom (i, f) =
    ( vLimit i
    . B.withBorderStyle B.unicodeBold
    . ((B.vBorder <=> bl) <+>)
    . (<+> (B.vBorder <=> br))
    . (<=> B.hBorder)
    . padRight Max
    ) @@= f

-- | Creates the left and right borders for the middle elements of the group.
groupBorderMid :: (Int, s -> FormFieldState s e n) -> (s -> FormFieldState s e n)
groupBorderMid (i, f) =
    ( vLimit i
    . B.withBorderStyle B.unicodeBold
    . (B.vBorder <+>)
    . (<+> B.vBorder)
    . padRight Max
    ) @@= f

-- | Creates the border around the only one element.
groupAllBorders :: String -> (Int, s -> FormFieldState s e n) -> (s -> FormFieldState s e n)
groupAllBorders groupName (i, f) =
    ( vLimit i
    . B.withBorderStyle B.unicodeBold
    . B.borderWithLabel (str groupName)
    . padRight Max
    ) @@= f


-- | Helpers for the correct border lines.
tl, tr, bl, br :: Widget n
tl = B.joinableBorder (Edges False True False True)
tr = B.joinableBorder (Edges False True True False)
bl = B.joinableBorder (Edges True False False True)
br = B.joinableBorder (Edges True False True False)
