{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types             #-}
{-# LANGUAGE TemplateHaskell        #-}

{- |
Copyright: (c) 2018-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module contains data types to work with application form. 'SummonKit' is
the data type containing the values manipulated by the fields in the form.
-}

module Summoner.Tui.Kit
       ( -- * Data types
         SummonKit (..)
       , User (..)
       , Project (..)
       , ProjectMeta (..)
       , GitHub (..)
       , renderWidgetTree
       , configToSummonKit
       , finalSettings

         -- * Lenses
         -- ** SummonKit
       , user
       , project
       , cabal
       , stack
       , projectMeta
       , gitHub
       , extensions
       , ghcOptions
       , stylish
       , contributing
       , offline
       , shouldSummon
       , configFile

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
       , actions
       , travis
       , appVeyor
       ) where

import Lens.Micro (Lens', lens, (.~), (^.))
import Lens.Micro.TH (makeFields)

import Summoner.Config (Config, ConfigP (..))
import Summoner.CustomPrelude (CustomPrelude (..))
import Summoner.Decision (Decision (..))
import Summoner.Default (currentYear, defaultDescription, defaultGHC)
import Summoner.GhcVer (GhcVer)
import Summoner.License (LicenseName (..), customizeLicense, fetchLicense)
import Summoner.Settings (Settings (..))
import Summoner.Source (Source, fetchSource)
import Summoner.Template (createProjectTemplate)
import Summoner.Tree (TreeFs, showTree)

import qualified Data.List as List (delete)
import qualified Data.Text as T


-- | Global TUI state.
data SummonKit = SummonKit
    { summonKitUser         :: !User
    , summonKitProject      :: !Project
    , summonKitCabal        :: !Bool
    , summonKitStack        :: !Bool
    , summonKitProjectMeta  :: !ProjectMeta
    , summonKitGitHub       :: !GitHub
    , summonKitExtensions   :: ![Text]  -- ^ Can be recieved from the config file.
    , summonKitGhcOptions   :: ![Text]  -- ^ Can be recieved from the config file.
    , summonKitGitignore    :: ![Text]  -- ^ Received from the config file.
    , summonKitStylish      :: !(Maybe Source)  -- ^ Can be recieved from the config file.
    , summonKitContributing :: !(Maybe Source)  -- ^ Can be recieved from the config file.
    , summonKitOffline      :: !Bool
    , summonKitShouldSummon :: !Decision  -- ^ Check if project needs to be created.
    , summonKitConfigFile   :: !(Maybe FilePath)  -- ^ Just if configuration file was used.
    , summonKitExtraFiles   :: ![TreeFs]  -- ^ Extra files
    } deriving stock (Show)

-- | User information.
data User = User
    { userOwner    :: !Text  -- ^ GitHub user or organization name.
    , userFullName :: !Text
    , userEmail    :: !Text
    } deriving stock (Show)

-- | Project related information
data Project = Project
    { projectRepo     :: !Text  -- ^ Project name.
    , projectDesc     :: !Text  -- ^ Short project description.
    , projectCategory :: !Text  -- ^ Comma-separated. See @Hackage@ for existing category list.
    , projectLicense  :: !LicenseName
    } deriving stock (Show)

-- | Project meta information.
data ProjectMeta = ProjectMeta
    { projectMetaLib           :: !Bool
    , projectMetaExe           :: !Bool
    , projectMetaTest          :: !Bool
    , projectMetaBench         :: !Bool
    , projectMetaGhcs          :: ![GhcVer]  -- ^ Default GHC version is always added.
    , projectMetaPreludeName   :: !Text
    , projectMetaPreludeModule :: !Text
    } deriving stock (Show)

-- | Github specific information.
data GitHub = GitHub
    { gitHubEnabled  :: !Bool
    , gitHubNoUpload :: !Bool  -- ^ Do not upload to GitHub, only local initialization.
    , gitHubPrivate  :: !Bool
    , gitHubActions  :: !Bool
    , gitHubTravis   :: !Bool
    , gitHubAppVeyor :: !Bool
    } deriving stock (Show)

makeFields ''SummonKit
makeFields ''User
makeFields ''Project
makeFields ''ProjectMeta
makeFields ''GitHub

-- | Lens for 'Maybe' 'LicenseName' in 'SummonKit'.
maybeLicense :: Lens' SummonKit (Maybe LicenseName)
maybeLicense = lens getL setL
  where
    getL :: SummonKit -> Maybe LicenseName
    getL = Just . projectLicense . summonKitProject

    setL :: SummonKit -> Maybe LicenseName -> SummonKit
    setL sk mbL = case mbL of
        Just l  -> sk & project . license .~ l
        Nothing -> sk

-- | Converts 'SummonKit' to main 'Settings' data type.
summonKitToSettings :: SummonKit -> Settings
summonKitToSettings sk = Settings
    { settingsRepo           = T.strip $ sk ^. project . repo
    , settingsOwner          = T.strip $ sk ^. user . owner
    , settingsDescription    = T.strip . unwords . lines $ sk ^. project . desc
    , settingsFullName       = T.strip $ sk ^. user . fullName
    , settingsEmail          = T.strip $ sk ^. user . email
    , settingsYear           = "20!8"
    , settingsCategories     = T.strip $ sk ^. project . category
    , settingsLicenseName    = sk ^. project . license
    , settingsLicenseText    = ""
    , settingsGitHub         = isGitHub
    , settingsGhActions      = isGitHub && sk ^. gitHub . actions && sk ^. cabal
    , settingsPrivate        = isGitHub && sk ^. gitHub . private
    , settingsTravis         = isGitHub && sk ^. gitHub . travis
    , settingsAppVeyor       = isGitHub && sk ^. gitHub . appVeyor
    , settingsIsLib          = sk ^. projectMeta . lib
    , settingsIsExe          = sk ^. projectMeta . exe
    , settingsTest           = sk ^. projectMeta . test
    , settingsBench          = sk ^. projectMeta . bench
    , settingsTestedVersions = sortNub $ defaultGHC : (sk ^. projectMeta . ghcs)
    , settingsBaseType       = baseT
    , settingsPrelude        = cP
    , settingsExtensions     = sk ^. extensions
    , settingsGhcOptions     = sk ^. ghcOptions
    , settingsGitignore      = sk ^. gitignore
    , settingsCabal          = sk ^. cabal
    , settingsStack          = sk ^. stack
    , settingsStylish        = "" <$ sk ^. stylish
    , settingsContributing   = "" <$ sk ^. contributing
    , settingsNoUpload       = sk ^. gitHub . noUpload
    , settingsFiles          = sk ^. extraFiles
    }
  where
    isGitHub :: Bool
    isGitHub = sk ^. gitHub . enabled

    baseT :: Text
    cP ::  Maybe CustomPrelude
    (baseT, cP) =
        let cpPackage = T.strip $ sk ^. projectMeta . preludeName
            cpModule  = T.strip $ sk ^. projectMeta . preludeModule
        in if ("" /= cpPackage) && ("" /= cpModule)
           then ("base-noprelude", Just CustomPrelude{..})
           else ("base", Nothing)

-- | Gets 'Settings' on successful application complition.
finalSettings :: SummonKit -> IO Settings
finalSettings sk = do
    year <- currentYear
    let licenseName = sk ^. project . license
    fetchedLicense <- fetchLicense licenseName
    let licenseText = customizeLicense
            licenseName
            fetchedLicense
            (sk ^. user . fullName)
            year

    let fetch = maybe (pure Nothing) (fetchSource (sk ^. offline))
    sStylish      <- fetch $ sk ^. stylish
    sContributing <- fetch $ sk ^. contributing

    pure (summonKitToSettings sk)
        { settingsYear = year
        , settingsLicenseText = licenseText
        , settingsStylish = sStylish
        , settingsContributing = sContributing
        }

-- | Gets the initial 'SummonKit' from the given 'Config'.
configToSummonKit
    :: Text  -- ^ Given project name
    -> Bool    -- ^  @offline@ mode option
    -> Maybe FilePath  -- ^ Configuration file used
    -> [TreeFs]  -- ^ Extra files
    -> Config  -- ^ Given configurations.
    -> SummonKit
configToSummonKit cRepo cOffline cConfigFile files Config{..} = SummonKit
    { summonKitUser  = User
        { userOwner    = cOwner
        , userFullName = cFullName
        , userEmail    = cEmail
        }
    , summonKitProject = Project
        { projectRepo     = cRepo
        , projectDesc     = defaultDescription
        , projectCategory = ""
        , projectLicense  = if cOffline then None else cLicense
        }
    , summonKitProjectMeta = ProjectMeta
        { projectMetaLib = kitLib
        , projectMetaExe = kitExe
        , projectMetaTest = toBool cTest
        , projectMetaBench = toBool cBench
        , projectMetaGhcs = List.delete defaultGHC cGhcVer
        , projectMetaPreludeName = kitPreludeName
        , projectMetaPreludeModule = kitPreludeModule
        }
    , summonKitCabal = kitCabal
    , summonKitStack = kitStack
    , summonKitGitHub = GitHub
        { gitHubEnabled  = cGitHub /= Nop
        , gitHubNoUpload = getAny cNoUpload || cOffline
        , gitHubPrivate  = toBool cPrivate
        , gitHubActions  = (cGitHub /= Nop) && (cGhActions /= Nop) && kitCabal
        , gitHubTravis   = (cGitHub /= Nop) && (cTravis /= Nop)
        , gitHubAppVeyor = toBool cAppVey
        }
    , summonKitExtensions   = cExtensions
    , summonKitGhcOptions   = cGhcOptions
    , summonKitGitignore    = cGitignore
    , summonKitStylish      = getLast cStylish
    , summonKitContributing = getLast cContributing
    , summonKitOffline      = cOffline
    , summonKitShouldSummon = Nop
    , summonKitConfigFile   = cConfigFile
    , summonKitExtraFiles   = files
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

-- | Shows the Widget with the generated project structure tree.
renderWidgetTree :: SummonKit -> Text
renderWidgetTree = showTree False . createProjectTemplate . summonKitToSettings
