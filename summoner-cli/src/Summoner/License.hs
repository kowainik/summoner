{- |
Module                  : Summoner.License
Copyright               : (c) 2017-2021 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Data types that represent license names and license content and functions
to work with them.
-}

module Summoner.License
       ( LicenseName(..)
       , License(..)
       , customizeLicense
       , githubLicenseQueryNames
       , parseLicenseName
       , fetchLicense
       , fetchLicenseCustom
       , licenseShortDesc
       , showLicenseWithDesc
       ) where

import Colourista (errorMessage)
import Data.Aeson.Micro (FromJSON (..), decodeStrict, withObject, (.:))
import Shellmet (($|))

import qualified Data.Text as T
import qualified Text.Show as TS


-- | Licenses supported by @summoner@.
data LicenseName
    = MIT
    | BSD2
    | BSD3
    | GPL2
    | GPL3
    | LGPL21
    | LGPL3
    | AGPL3
    | Apache20
    | MPL20
    | ISC
    | NONE
    deriving stock (Eq, Ord, Enum, Bounded, Generic)

instance Show LicenseName where
    show MIT      = "MIT"
    show BSD2     = "BSD-2-Clause"
    show BSD3     = "BSD-3-Clause"
    show GPL2     = "GPL-2.0-only"
    show GPL3     = "GPL-3.0-only"
    show LGPL21   = "LGPL-2.1-only"
    show LGPL3    = "LGPL-3.0-only"
    show AGPL3    = "AGPL-3.0-only"
    show Apache20 = "Apache-2.0"
    show MPL20    = "MPL-2.0"
    show ISC      = "ISC"
    show NONE     = "NONE"

newtype License = License
    { unLicense :: Text
    } deriving stock (Show, Generic)
      deriving newtype (IsString)

instance FromJSON License where
    parseJSON = withObject "License" $ \o -> License <$> o .: "body"

-- | Used for downloading the license text form @Github@.
githubLicenseQueryNames :: LicenseName -> Text
githubLicenseQueryNames = \case
    MIT      -> "mit"
    BSD2     -> "bsd-2-clause"
    BSD3     -> "bsd-3-clause"
    GPL2     -> "gpl-2.0"
    GPL3     -> "gpl-3.0"
    LGPL21   -> "lgpl-2.1"
    LGPL3    -> "lgpl-3.0"
    AGPL3    -> "agpl-3.0"
    Apache20 -> "apache-2.0"
    MPL20    -> "mpl-2.0"
    ISC      -> "isc"
    NONE     -> "none"

parseLicenseName :: Text -> Maybe LicenseName
parseLicenseName = inverseMap show

-- | Replaces name/year placeholders with the actual data.
customizeLicense :: LicenseName -> License -> Text -> Text -> License
customizeLicense l license@(License licenseText) nm year
    | l `elem` [MIT, BSD2, BSD3, ISC] = License updatedLicenseText
    | otherwise                  = license
  where
    updatedLicenseText :: Text
    updatedLicenseText =
        let (beforeY, withY) = T.span (/= '[') licenseText
            afterY           = T.tail $ T.dropWhile (/= ']') withY
            (beforeN, withN) = T.span (/= '[') afterY
            afterN           = T.tail $ T.dropWhile (/= ']') withN
        in  beforeY <> year <> beforeN <> nm <> afterN

-- | Download the given LICENSE text as it is from GitHub API.
fetchLicense :: LicenseName -> IO License
fetchLicense NONE = pure $ License $ licenseShortDesc NONE
fetchLicense name = do
    let licenseLink = "https://api.github.com/licenses/" <> githubLicenseQueryNames name
    licenseJson <- "curl" $|
        [ licenseLink
        , "-H"
        , "Accept: application/vnd.github.drax-preview+json"
        , "--silent"
        , "--fail"
        ]

    whenNothing (decodeStrict @License $ encodeUtf8 licenseJson) $ do
        errorMessage $ "Error downloading license: " <> show name
        putTextLn $ "Fetched content:\n" <> licenseJson
        exitFailure

{- | Fetches the license by given name and customises user information where
applicable.
-}
fetchLicenseCustom :: LicenseName -> Text -> Text -> IO License
fetchLicenseCustom license fullName year = do
    licenseText <- fetchLicense license
    pure $ customizeLicense license licenseText fullName year

-- | Show short information for the 'LicenseName'.
licenseShortDesc :: LicenseName -> Text
licenseShortDesc = \case
    MIT      -> "MIT license"
    BSD2     -> "2-clause BSD license"
    BSD3     -> "3-clause BSD license"
    GPL2     -> "GNU General Public License, version 2"
    GPL3     -> "GNU General Public License, version 3"
    LGPL21   -> "GNU Lesser General Public License, version 2.1"
    LGPL3    -> "GNU Lesser General Public License, version 3"
    AGPL3    -> "GNU Affero General Public License, version 3"
    Apache20 -> "Apache License, version 2.0"
    MPL20    -> "Mozilla Public License, version 2.0"
    ISC      -> "Internet Systems Consortium"
    NONE -> "License file won't be added. The package may not be legally \
        \modified or redistributed by anyone but the rightsholder"

-- | Show license name along with its short description.
showLicenseWithDesc :: LicenseName -> Text
showLicenseWithDesc l = show l <> ": " <> licenseShortDesc l
