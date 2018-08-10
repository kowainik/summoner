module Summoner.License
       ( License (..)
       , LicenseBody (..)
       , customizeLicense
       , githubLicenseQueryNames
       , parseLicense
       , showLicense
       ) where

import Relude
import Relude.Extra.Enum (inverseMap)

import Data.Aeson (FromJSON (..), withObject, (.:))

import qualified Data.Text as T

----------------------------------------------------------------------------
-- License
----------------------------------------------------------------------------

data License = MIT
             | BSD2
             | BSD3
             | GPL2
             | GPL3
             | LGPL21
             | LGPL3
             | AGPL3
             | Apache20
             | MPL20
             deriving (Eq, Ord, Show, Enum, Bounded, Generic)

githubLicenseQueryNames :: License -> Text
githubLicenseQueryNames MIT      = "mit"
githubLicenseQueryNames BSD2     = "bsd-2-clause"
githubLicenseQueryNames BSD3     = "bsd-3-clause"
githubLicenseQueryNames GPL2     = "gpl-2.0"
githubLicenseQueryNames GPL3     = "gpl-3.0"
githubLicenseQueryNames LGPL21   = "lgpl-2.1"
githubLicenseQueryNames LGPL3    = "lgpl-3.0"
githubLicenseQueryNames AGPL3    = "agpl-3.0"
githubLicenseQueryNames Apache20 = "apache-2.0"
githubLicenseQueryNames MPL20    = "mpl-2.0"

showLicense :: License -> Text
showLicense MIT      = "MIT"
showLicense BSD2     = "BSD2"       
showLicense BSD3     = "BSD3"       
showLicense GPL2     = "GPL-2"      
showLicense GPL3     = "GPL-3"      
showLicense LGPL21   = "LGPL-2.1"   
showLicense LGPL3    = "LGPL-3"     
showLicense AGPL3    = "AGPL-3"     
showLicense Apache20 = "Apache-2.0" 
showLicense MPL20    = "MPL-2.0"

parseLicense :: Text -> Maybe License
parseLicense = inverseMap showLicense

newtype LicenseBody = LicenseBody { unBody :: Text }
    deriving (IsString, Show, Generic)

instance FromJSON LicenseBody where
    parseJSON = withObject "License" $ \o -> LicenseBody <$> o .: "body"

customizeLicense :: Text -> Text -> Text -> Text -> Text
customizeLicense l t nm year
    | l `elem` words "MIT BSD2 BSD3" = updateLicenseText
    | otherwise = t
  where
    updateLicenseText =
        let (beforeY, withY) = T.span (/= '[') t
            afterY = T.tail $ T.dropWhile (/= ']') withY
            (beforeN, withN) = T.span (/= '[') afterY
            afterN = T.tail $ T.dropWhile (/= ']') withN
        in beforeY <> year <> beforeN <> nm <> afterN
