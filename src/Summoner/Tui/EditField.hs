{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Summoner.Tui.EditField
       ( EditField (..)
       , toEditFields

         -- * Lenses
       , labelL
       , textL
       , editFieldL
       ) where

import Lens.Micro (Lens', ix, lens, (.~))
import Lens.Micro.TH (makeFields)
import Relude.Unsafe as Unsafe


data EditField = EditField
    { editFieldLabelL :: Text
    , editFieldTextL  :: Text
    } deriving (Show)

makeFields ''EditField

toEditFields :: [Text] -> [EditField]
toEditFields = map (`EditField` "")

-- | Creates lense for 'CheckBox'es list at the given position.
editFieldL :: Int -> Lens' [EditField] Text
editFieldL i = lens getAt setAt
  where
    getAt :: [EditField] -> Text
    getAt l = editFieldTextL $ Unsafe.at i l

    setAt :: [EditField] -> Text -> [EditField]
    setAt l newText = l & ix i . textL .~ newText
