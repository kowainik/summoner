{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Summoner configurations.

module Summoner.Config
       ( ConfigP (..)

       , PartialConfig
       , Config
       , defaultConfig
       , finalise

       , loadFileConfig
       ) where

import Control.Exception (throwIO)
import Data.List (lookup)
import Data.Monoid (Last (..))
import Generics.Deriving.Monoid (GMonoid, gmemptydefault)
import Generics.Deriving.Semigroup (GSemigroup, gsappenddefault)
import Toml (ValueType (TString), matchText)
import Toml.Bi (BiToml, dimapBijection, (.=))
import Toml.Bi.Combinators (Valuer (..))
import Toml.PrefixTree (Key)

import Summoner.License (License (..))
import Summoner.ProjectData (Decision (..), GhcVer (..), parseGhcVer, showGhcVer)
import Summoner.Validation (Validation (..))

import qualified Text.Show as Show
import qualified Toml

data Phase = Partial | Final

-- | Potentially incomplete configuration.
data ConfigP (p :: Phase) = Config
    { cOwner    :: p :- Text
    , cFullName :: p :- Text
    , cEmail    :: p :- Text
    , cLicense  :: p :- License
    , cGhcVer   :: p :- [GhcVer]
    , cGitHub   :: Decision
    , cTravis   :: Decision
    , cAppVey   :: Decision
    , cPrivate  :: Decision
    , cScript   :: Decision
    , cLib      :: Decision
    , cExe      :: Decision
    , cTest     :: Decision
    , cBench    :: Decision
    } deriving (Generic)

deriving instance (GSemigroup (p :- Text), GSemigroup (p :- License), GSemigroup (p :- [GhcVer])) => GSemigroup (ConfigP p)
deriving instance (GMonoid (p :- Text), GMonoid (p :- License), GMonoid (p :- [GhcVer])) => GMonoid (ConfigP p)

infixl 3 :-
type family phase :- field where
    'Partial :- field = Last field
    'Final   :- field = field

-- | Incomplete configurations.
type PartialConfig = ConfigP 'Partial

-- | Complete configurations.
type Config = ConfigP 'Final

instance Semigroup PartialConfig where
    (<>) = gsappenddefault

instance Monoid PartialConfig where
    mempty = gmemptydefault
    mappend = (<>)

-- | Default 'Config' configurations.
defaultConfig :: PartialConfig
defaultConfig = Config
    { cOwner    = Last (Just "kowainik")
    , cFullName = Last (Just "Kowainik")
    , cEmail    = Last (Just "xrom.xkov@gmail.com")
    , cLicense  = Last (Just $ License "MPL-2.0")
    , cGhcVer   = Last (Just [])
    , cGitHub   = Idk
    , cTravis   = Idk
    , cAppVey   = Idk
    , cPrivate  = Idk
    , cScript   = Idk
    , cLib      = Idk
    , cExe      = Idk
    , cTest     = Idk
    , cBench    = Idk
    }

-- | Identifies how to read 'Config' data from the @.toml@ file.
configT :: BiToml PartialConfig
configT = Config
    <$> lastP Toml.str  "owner"       .= cOwner
    <*> lastP Toml.str  "fullName"    .= cFullName
    <*> lastP Toml.str  "email"       .= cEmail
    <*> lastP license   "license"     .= cLicense
    <*> lastP ghcVerArr "ghcVersions" .= cGhcVer
    <*> decision        "github"      .= cGitHub
    <*> decision        "travis"      .= cTravis
    <*> decision        "appveyor"    .= cAppVey
    <*> decision        "private"     .= cPrivate
    <*> decision        "bscript"     .= cScript
    <*> decision        "lib"         .= cLib
    <*> decision        "exe"         .= cExe
    <*> decision        "test"        .= cTest
    <*> decision        "bench"       .= cBench
  where
    lastP :: (Key -> BiToml a) -> Key -> BiToml (Last a)
    lastP f = dimapBijection getLast Last . Toml.maybeP f

    ghcVerV :: Valuer 'TString GhcVer
    ghcVerV = Valuer (matchText >=> parseGhcVer) (Toml.String . showGhcVer)

    ghcVerArr :: Key -> BiToml [GhcVer]
    ghcVerArr = Toml.arrayOf ghcVerV

    license :: Key -> BiToml License
    license =  dimapBijection unLicense License . Toml.str

    decision :: Key -> BiToml Decision
    decision = dimapBijection fromDecision toDecision . Toml.maybeP Toml.bool

    decisionMaybe :: [(Decision, Maybe Bool)]
    decisionMaybe = [ (Idk, Nothing)
                    , (Yes, Just True)
                    , (Nop, Just False)
                    ]

    fromDecision :: Decision -> Maybe Bool
    fromDecision d = join $ lookup d decisionMaybe

    toDecision :: Maybe Bool -> Decision
    toDecision m = fromMaybe (error "Impossible") $ lookup m $ map swap decisionMaybe


-- | Make sure that all the required configurations options were specified.
finalise :: PartialConfig -> Validation [Text] Config
finalise Config{..} = Config
    <$> fin  "owner"      cOwner
    <*> fin  "fullName"   cFullName
    <*> fin  "email"      cEmail
    <*> fin  "license"    cLicense
    <*> fin  "ghcersions" cGhcVer
    <*> pure cGitHub
    <*> pure cTravis
    <*> pure cAppVey
    <*> pure cPrivate
    <*> pure cScript
    <*> pure cLib
    <*> pure cExe
    <*> pure cTest
    <*> pure cBench
  where
    fin name = maybe (Failure ["Missing field: " <> name]) Success . getLast

-- | Read configuration from the given file and return it in data type.
loadFileConfig :: MonadIO m => FilePath -> m PartialConfig
loadFileConfig filePath = (Toml.encode configT <$> readFile filePath) >>= liftIO . errorWhenLeft
  where
    errorWhenLeft :: Either Toml.EncodeException PartialConfig -> IO PartialConfig
    errorWhenLeft (Left e)   = throwIO $ LoadTomlException filePath $ show e
    errorWhenLeft (Right pc) = pure pc

data LoadTomlException = LoadTomlException FilePath String

instance Show.Show LoadTomlException where
    show (LoadTomlException filePath msg) = "Couldnt parse file " ++ filePath ++ ": " ++ show msg

instance Exception LoadTomlException
