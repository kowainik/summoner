{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

{- | This module contains data types to work with application form.
'SummonKit' is the data type containing the values manipulated by the fields
in the form.
-}

module Summoner.Tui.Kit
       ( -- * Data types
         SummonKit (..)
       , initialSummonKit

         -- * Lenses
       , userL
       , test1L
       , test2L
       ) where

import Lens.Micro.TH (makeFields)

import Summoner.Tui.CheckBox (CheckBox, toCheckBoxes)
import Summoner.Tui.EditField (EditField, toEditFields)


data Test1 = Test1 Text Text
    deriving (Show)
data Test2 = Test2 Text Int
    deriving (Show)

-- | Global TUI state.
data SummonKit = SummonKit
    { summonKitUserL  :: [EditField]
    , summonKitTest1L :: [CheckBox Test1]
    , summonKitTest2L :: [CheckBox Test2]
    } deriving (Show)

makeFields ''SummonKit

-- | Initial global state of the tui.
initialSummonKit :: SummonKit
initialSummonKit = SummonKit
    { summonKitUserL  = toEditFields [ "Owner", "Full name", "Email"]
    , summonKitTest1L = toCheckBoxes [Test1 "a" "a", Test1 "b" "b"]
    , summonKitTest2L = toCheckBoxes [Test2 "a2" 1, Test2 "b2" 2]
    }
