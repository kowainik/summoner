{-# LANGUAGE Rank2Types #-}

module Summoner.Tui.CheckBox
       ( -- * Data type
         CheckBox (..)
       , toCheckBoxes

         -- * Lenses
       , checkBoxL
       ) where

import Lens.Micro (Lens', ix, lens, (.~))

import Relude.Unsafe as Unsafe


-- | Represents the checkbox for some data.
data CheckBox a = CheckBox
    { checkboxData :: a
    , checkboxFlag :: Bool
    } deriving (Show)

-- | Creates the list of the 'CheckBox'es from the list of data with the 'False'
-- flags.
toCheckBoxes :: [a] -> [CheckBox a]
toCheckBoxes = map (`CheckBox` False)

-- | Lens for 'checkboxFlag' field of the 'CheckBox' data type.
flagL :: Lens' (CheckBox a) Bool
flagL = lens checkboxFlag $ \checkbox newFlag -> checkbox { checkboxFlag = newFlag }

-- | Creates lense for 'CheckBox'es list at the given position.
checkBoxL :: Int -> Lens' [CheckBox a] Bool
checkBoxL i = lens getAt setAt
  where
    getAt :: [CheckBox a] -> Bool
    getAt l = checkboxFlag $ Unsafe.at i l

    setAt :: [CheckBox a] -> Bool -> [CheckBox a]
    setAt l newBool = l & ix i . flagL .~ newBool
