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
       , project
       , cabal
       , stack
       , projectMeta
       , gitHub

         -- ** User
       , owner
       , fullName
       , email

         -- ** Project
       , repo
       , desc
       , category
       , license
       , maybeLicense

         -- ** ProjectMeta
       , lib
       , exe
       , test
       , bench
       , ghcs
       , preludeName
       , preludeModule

         -- ** GitHub
       , enabled
       , private
       , travis
       , appVeyor
       ) where

import Lens.Micro (Lens', lens, (.~))
import Lens.Micro.TH (makeFields)

import Summoner.Default (defaultGHC)
import Summoner.GhcVer (GhcVer)
import Summoner.License (LicenseName (..))


-- | Global TUI state.
data SummonKit = SummonKit
    { summonKitUser        :: User
    , summonKitProject     :: Project
    , summonKitCabal       :: Bool
    , summonKitStack       :: Bool
    , summonKitProjectMeta :: ProjectMeta
    , summonKitGitHub      :: GitHub
    } deriving (Show)

data User = User
    { userOwner    :: Text
    , userFullName :: Text
    , userEmail    :: Text
    } deriving (Show)

data Project = Project
    { projectRepo     :: Text
    , projectDesc     :: Text
    , projectCategory :: Text
    , projectLicense  :: LicenseName
    } deriving (Show)

data ProjectMeta = ProjectMeta
    { projectMetaLib           :: Bool
    , projectMetaExe           :: Bool
    , projectMetaTest          :: Bool
    , projectMetaBench         :: Bool
    , projectMetaGhcs          :: [GhcVer]
    , projectMetaPreludeName   :: Text
    , projectMetaPreludeModule :: Text
    } deriving (Show)

data GitHub = GitHub
    { gitHubEnabled  :: Bool
    , gitHubPrivate  :: Bool
    , gitHubTravis   :: Bool
    , gitHubAppVeyor :: Bool
    } deriving (Show)

-- | Initial global state of the tui.
initialSummonKit :: SummonKit
initialSummonKit = SummonKit
    { summonKitUser  = User
        { userOwner = ""
        , userFullName = ""
        , userEmail = ""
        }
    , summonKitProject = Project
        { projectRepo = ""
        , projectDesc = ""
        , projectCategory = ""
        , projectLicense = MIT
        }
    , summonKitProjectMeta = ProjectMeta
        { projectMetaLib = True
        , projectMetaExe = False
        , projectMetaTest = True
        , projectMetaBench = False
        , projectMetaGhcs = [defaultGHC]
        , projectMetaPreludeName = ""
        , projectMetaPreludeModule = ""
        }
    , summonKitCabal = True
    , summonKitStack = True
    , summonKitGitHub = GitHub
        { gitHubEnabled  = True
        , gitHubPrivate  = False
        , gitHubTravis   = False
        , gitHubAppVeyor = False
        }
    }

makeFields ''SummonKit
makeFields ''User
makeFields ''Project
makeFields ''ProjectMeta
makeFields ''GitHub

maybeLicense :: Lens' SummonKit (Maybe LicenseName)
maybeLicense = lens getL setL
  where
    getL :: SummonKit -> Maybe LicenseName
    getL = Just . projectLicense . summonKitProject

    setL :: SummonKit -> Maybe LicenseName -> SummonKit
    setL sk mbL = case mbL of
        Just l  -> sk & project . license .~ l
        Nothing -> sk
