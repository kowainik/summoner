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
       , tools
       ) where

import Lens.Micro.TH (makeFields)

import Summoner.Tui.CheckBox (CheckBox, toCheckBoxes)
import Summoner.Tui.EditField (EditField, toEditFields)


data Test1 = Test1 Text Text
    deriving (Show)

-- | Global TUI state.
data SummonKit = SummonKit
    { summonKitUserL  :: [EditField]
    , summonKitTest1L :: [CheckBox Test1]
    , summonKitTools  :: [CheckBox Tool]
    } deriving (Show)

-- | Represents the build tool that can be used in the generated project.
data Tool
    = Cabal
    | Stack
  deriving (Show)

makeFields ''SummonKit

-- | Initial global state of the tui.
initialSummonKit :: SummonKit
initialSummonKit = SummonKit
    { summonKitUserL  = toEditFields [ "Owner", "Full name", "Email"]
    , summonKitTest1L = toCheckBoxes [Test1 "a" "a", Test1 "b" "b"]
    , summonKitTools  = toCheckBoxes [Cabal, Stack]
    }
