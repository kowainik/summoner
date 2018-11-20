{- | This modules adds necessary functions for Forms, that are not covered
in @brick@ library.
-}
module Summoner.Tui.Forms
       ( textField
       ) where

import Brick (Widget, txt, vBox, (<+>))
import Brick.Forms (FormFieldState (..))
import Lens.Micro (Lens', lens)

-- | Text field in the 'Brick.Forms.Form'.
textField :: forall s e n . Text -> s -> FormFieldState s e n
textField t _ = FormFieldState
    { formFieldState = ()
    , formFieldLens = textFakeLens
    , formFields = []
    , formFieldRenderHelper = renderText
    , formFieldConcat = vBox
    }
  where
    -- looool
    textFakeLens :: Lens' s ()
    textFakeLens = lens (const ()) (\s () -> s)

    renderText :: Widget n -> Widget n
    renderText w = txt t <+> w
