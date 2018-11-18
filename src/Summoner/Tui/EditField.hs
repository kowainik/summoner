{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module Summoner.Tui.EditField
       ( EditField (..)
       , toEditFields

         -- * Lenses
       , labelL
       , textL
       ) where

import Lens.Micro.TH (makeFields)


data EditField = EditField
    { editFieldLabelL :: Text
    , editFieldTextL  :: Text
    } deriving (Show)

makeFields ''EditField

toEditFields :: [Text] -> [EditField]
toEditFields = map (`EditField` "")
