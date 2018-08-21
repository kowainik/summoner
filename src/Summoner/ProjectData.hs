{-# LANGUAGE ViewPatterns #-}

module Summoner.ProjectData
       ( ProjectData (..)
       , CustomPrelude (..)
       ) where

import Relude

import Summoner.GhcVer (GhcVer)
import Summoner.License (LicenseName)


data CustomPrelude = Prelude
    { cpPackage :: Text
    , cpModule  :: Text
    } deriving (Show, Eq)

-- | Data needed for project creation.
data ProjectData = ProjectData
    { repo           :: Text   -- ^ repository name
    , owner          :: Text   -- ^ github username
    , description    :: Text   -- ^ project description
    , nm             :: Text   -- ^ full name
    , email          :: Text   -- ^ e-mail
    , year           :: Text   -- ^ year
    , category       :: Text   -- ^ project category
    , license        :: LicenseName   -- ^ type of license
    , licenseText    :: Text   -- ^ license text
    , github         :: Bool   -- ^ github repository
    , travis         :: Bool   -- ^ Travis CI integration
    , appVey         :: Bool   -- ^ AppVeyor CI integration
    , script         :: Bool   -- ^ build script
    , isLib          :: Bool   -- ^ is library
    , isExe          :: Bool   -- ^ is executable
    , test           :: Bool   -- ^ add tests
    , bench          :: Bool   -- ^ add benchmarks
    , testedVersions :: [GhcVer]  -- ^ ghc versions
    , base           :: Text -- ^ Base library to use
    , prelude        :: Maybe CustomPrelude  -- ^ custom prelude to be used
    , extensions     :: [Text] -- ^ default extensions
    , warnings       :: [Text] -- ^ default warnings
    , cabal          :: Bool
    , stack          :: Bool
    } deriving (Show)
