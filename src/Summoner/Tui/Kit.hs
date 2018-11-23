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
       , renderWidgetTree
       , configToSummonKit

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
       , noUpload
       , private
       , travis
       , appVeyor
       ) where

import Lens.Micro (Lens', lens, (.~), (^.))
import Lens.Micro.TH (makeFields)

import Summoner.Config (Config, ConfigP (..))
import Summoner.Decision (Decision (..))
import Summoner.GhcVer (GhcVer)
import Summoner.License (LicenseName (..))
import Summoner.Settings (CustomPrelude (..), Settings (..))
import Summoner.Source (Source)
import Summoner.Template (createProjectTemplate)
import Summoner.Tree (showTree)


-- | Global TUI state.
data SummonKit = SummonKit
    { summonKitUser         :: !User
    , summonKitProject      :: !Project
    , summonKitCabal        :: !Bool
    , summonKitStack        :: !Bool
    , summonKitProjectMeta  :: !ProjectMeta
    , summonKitGitHub       :: !GitHub
    , summonKitExtensions   :: [Text]
    , summonKitWarnings     :: [Text]
    , summonKitStylish      :: Maybe Source
    , summonKitContributing :: Maybe Source
    } deriving (Show)

data User = User
    { userOwner    :: !Text
    , userFullName :: !Text
    , userEmail    :: !Text
    } deriving (Show)

data Project = Project
    { projectRepo     :: !Text
    , projectDesc     :: !Text
    , projectCategory :: !Text
    , projectLicense  :: !LicenseName
    } deriving (Show)

data ProjectMeta = ProjectMeta
    { projectMetaLib           :: !Bool
    , projectMetaExe           :: !Bool
    , projectMetaTest          :: !Bool
    , projectMetaBench         :: !Bool
    , projectMetaGhcs          :: ![GhcVer]
    , projectMetaPreludeName   :: !Text
    , projectMetaPreludeModule :: !Text
    } deriving (Show)

data GitHub = GitHub
    { gitHubEnabled  :: !Bool
    , gitHubNoUpload :: !Bool
    , gitHubPrivate  :: !Bool
    , gitHubTravis   :: !Bool
    , gitHubAppVeyor :: !Bool
    } deriving (Show)

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

-- TODO: WIP need to fetch license in other func?
-- The following function is for tree rendering only.
-- | Converts 'SummonKit' to main 'Settings' data type.
summonKitToSettings :: SummonKit -> Settings
summonKitToSettings sk = Settings
    { settingsRepo           = sk ^. project . repo
    , settingsOwner          = sk ^. user . owner
    , settingsDescription    = sk ^. project . desc
    , settingsFullName       = sk ^. user . fullName
    , settingsEmail          = sk ^. user . email
    , settingsYear           = "20!8"
    , settingsCategories     = sk ^. project . category
    , settingsLicenseName    = sk ^. project . license
    , settingsLicenseText    = ""
    , settingsGitHub         = isGitHub
    , settingsTravis         = isGitHub && sk ^. gitHub . travis
    , settingsAppVeyor       = isGitHub && sk ^. gitHub . appVeyor
    , settingsIsLib          = sk ^. projectMeta . lib
    , settingsIsExe          = sk ^. projectMeta . exe
    , settingsTest           = sk ^. projectMeta . test
    , settingsBench          = sk ^. projectMeta . bench
    , settingsTestedVersions = sk ^. projectMeta . ghcs
    , settingsBaseType       = baseT
    , settingsPrelude        = cP
    , settingsExtensions     = []
    , settingsWarnings       = []
    , settingsCabal          = sk ^. cabal
    , settingsStack          = sk ^. stack
    , settingsStylish        = Nothing
    , settingsContributing   = Nothing
    }
  where
    isGitHub :: Bool
    isGitHub = sk ^. gitHub . enabled

    baseT :: Text
    cP ::  Maybe CustomPrelude
    (baseT, cP) =
        let cpPackage = sk ^. projectMeta . preludeName
            cpModule  = sk ^. projectMeta . preludeModule
        in if ("" /= cpPackage) && ("" /= cpModule)
           then ("base-noprelude", Just CustomPrelude{..})
           else ("base", Nothing)

-- | Gets the initial 'SummonKit' from the given 'Config'.
configToSummonKit
    :: Text  -- ^ Given project name
    -> Bool  -- ^ no @noUpload@ option (to not upload to @Github@).
    -> Config  -- ^ Given configurations.
    -> SummonKit
configToSummonKit cRepo cNoUpload Config{..} = SummonKit
    { summonKitUser  = User
        { userOwner    = cOwner
        , userFullName = cFullName
        , userEmail    = cEmail
        }
    , summonKitProject = Project
        { projectRepo     = cRepo
        , projectDesc     = ""
        , projectCategory = ""
        , projectLicense  = cLicense
        }
    , summonKitProjectMeta = ProjectMeta
        { projectMetaLib = kitLib
        , projectMetaExe = kitExe
        , projectMetaTest = toBool cTest
        , projectMetaBench = toBool cBench
        , projectMetaGhcs = cGhcVer
        , projectMetaPreludeName = kitPreludeName
        , projectMetaPreludeModule = kitPreludeModule
        }
    , summonKitCabal = kitCabal
    , summonKitStack = kitStack
    , summonKitGitHub = GitHub
        { gitHubEnabled  = cGitHub /= Nop
        , gitHubNoUpload = cNoUpload
        , gitHubPrivate  = toBool cPrivate
        , gitHubTravis   = (cGitHub /= Nop) && (cTravis /= Nop)
        , gitHubAppVeyor = toBool cAppVey
        }
    , summonKitExtensions   = cExtensions
    , summonKitWarnings     = cWarnings
    , summonKitStylish      = getLast cStylish
    , summonKitContributing = getLast cContributing
    }
  where
    kitCabal, kitStack, kitLib, kitExe :: Bool
    (kitCabal, kitStack) = decToBools (cCabal, cStack)
    (kitLib, kitExe) = decToBools (cLib, cExe)

    decToBools :: (Decision, Decision) -> (Bool, Bool)
    decToBools = \case
        (Idk, Idk) -> (True, True)
        (Yes, Idk) -> (True, False)
        (Idk, Yes) -> (False, True)
        (Nop, Idk) -> (False, True)
        (Idk, Nop) -> (True, False)
        (x, y)     -> (toBool x, toBool y)

    toBool :: Decision -> Bool
    toBool = \case
        Yes -> True
        Nop -> False
        Idk -> False

    kitPreludeName, kitPreludeModule :: Text
    (kitPreludeName, kitPreludeModule) = case getLast cPrelude of
        Nothing                -> ("", "")
        Just CustomPrelude{..} -> (cpPackage, cpModule)

renderWidgetTree :: SummonKit -> Text
renderWidgetTree = showTree False . createProjectTemplate . summonKitToSettings