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
         -- ** SummonKit
       , user
       , tools

         -- ** User
       , owner
       , fullName
       , email
       ) where

import Lens.Micro.TH (makeFields)

import Summoner.Tui.CheckBox (CheckBox, toCheckBoxes)


-- | Global TUI state.
data SummonKit = SummonKit
    { summonKitUser  :: User
    , summonKitTools :: [CheckBox Tool]
    } deriving (Show)

data User = User
    { userOwner    :: Text
    , userFullName :: Text
    , userEmail    :: Text
    } deriving (Show)

-- | Represents the build tool that can be used in the generated project.
data Tool
    = Cabal
    | Stack
    deriving (Show)


-- | Initial global state of the tui.
initialSummonKit :: SummonKit
initialSummonKit = SummonKit
    { summonKitUser  = User
        { userOwner = ""
        , userFullName = ""
        , userEmail = ""
        }
    , summonKitTools = toCheckBoxes [Cabal, Stack]
    }

makeFields ''SummonKit
makeFields ''User
