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
       , renderWidgetTree
       , summonKitToConfig

       , KitMode (..)
       , formName
       , leftColumnSize

         -- * Lenses
         -- ** SummonKit
       , mode
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

import Lens.Micro (Lens', lens, (.~), (^.))
import Lens.Micro.TH (makeFields)

import Summoner.Config (ConfigP (..), PartialConfig)
import Summoner.Decision (boolToDecision)
import Summoner.Default (defaultGHC)
import Summoner.GhcVer (GhcVer)
import Summoner.License (LicenseName (..))
import Summoner.Settings (CustomPrelude (..), Settings (..))
import Summoner.Template (createProjectTemplate)
import Summoner.Tree (showTree)


-- | Global TUI state.
data SummonKit = SummonKit
    { summonKitMode        :: !KitMode
    , summonKitUser        :: !User
    , summonKitProject     :: !Project
    , summonKitCabal       :: !Bool
    , summonKitStack       :: !Bool
    , summonKitProjectMeta :: !ProjectMeta
    , summonKitGitHub      :: !GitHub
    } deriving (Show)

data KitMode = New | Init
    deriving (Show, Eq)

formName :: KitMode -> String
formName New  = "Summon new project"
formName Init = "Initialize configuration"

leftColumnSize :: KitMode -> Int
leftColumnSize New  = 9
leftColumnSize Init = 6

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
    , gitHubPrivate  :: !Bool
    , gitHubTravis   :: !Bool
    , gitHubAppVeyor :: !Bool
    } deriving (Show)

-- | Initial global state of the tui.
initialSummonKit :: SummonKit
initialSummonKit = SummonKit
    { summonKitMode = Init
    , summonKitUser  = User
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
    , settingsGitHub         = sk ^. gitHub . enabled
    , settingsTravis         = sk ^. gitHub . travis
    , settingsAppVeyor       = sk ^. gitHub . appVeyor
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
    baseT :: Text
    baseT = maybe "base" (const "base-noprelude") cP

    cP ::  Maybe CustomPrelude
    cP = summonKitToPrelude sk

summonKitToPrelude :: SummonKit -> Maybe CustomPrelude
summonKitToPrelude sk =
    let cpPackage = sk ^. projectMeta . preludeName
        cpModule  = sk ^. projectMeta . preludeModule
    in if cpPackage /= "" && cpModule /= ""
       then Just CustomPrelude{..}
       else Nothing


summonKitToConfig :: SummonKit -> PartialConfig
summonKitToConfig sk = Config
    { cOwner        = guardLast (/= "") $ sk ^. user . owner
    , cFullName     = guardLast (/= "") $ sk ^. user . fullName
    , cEmail        = guardLast (/= "") $ sk ^. user . email
    , cLicense      = Last $ Just $ sk ^. project . license
    , cGhcVersions  = guardLast (not . null) $ sk ^. projectMeta . ghcs
    , cCabal        = boolToDecision $ sk ^. cabal
    , cStack        = boolToDecision $ sk ^. stack
    , cGitHub       = boolToDecision $ sk ^. gitHub . enabled
    , cPrivate      = boolToDecision $ sk ^. gitHub . private
    , cTravis       = boolToDecision $ sk ^. gitHub . travis
    , cAppVeyor     = boolToDecision $ sk ^. gitHub . appVeyor
    , cLib          = boolToDecision $ sk ^. projectMeta . lib
    , cExe          = boolToDecision $ sk ^. projectMeta . exe
    , cTest         = boolToDecision $ sk ^. projectMeta . test
    , cBench        = boolToDecision $ sk ^. projectMeta . bench
    , cPrelude      = Last $ summonKitToPrelude sk
    , cExtensions   = []
    , cWarnings     = []
    , cStylish      = mempty
    , cContributing = mempty
    }
  where
    guardLast :: (a -> Bool) -> a -> Last a
    guardLast p a = if p a then pure a else mempty

renderWidgetTree :: SummonKit -> Text
renderWidgetTree = showTree . createProjectTemplate . summonKitToSettings
