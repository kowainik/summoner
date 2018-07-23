module Summoner.License
       ( License (..)
       , customizeLicense
       , licenseNames
       , githubLicenseQueryNames
       ) where

import Relude

import Data.Aeson (FromJSON (..), withObject, (.:))

import qualified Data.Text as T

----------------------------------------------------------------------------
-- License
----------------------------------------------------------------------------

licenseNames :: [License]
licenseNames = map fst githubLicenseQueryNames

githubLicenseQueryNames :: [(License, Text)]
githubLicenseQueryNames =
    [ ("MIT",        "mit")
    , ("BSD2",       "bsd-2-clause")
    , ("BSD3",       "bsd-3-clause")
    , ("GPL-2",      "gpl-2.0")
    , ("GPL-3",      "gpl-3.0")
    , ("LGPL-2.1",   "lgpl-2.1")
    , ("LGPL-3",     "lgpl-3.0")
    , ("AGPL-3",     "agpl-3.0")
    , ("Apache-2.0", "apache-2.0")
    , ("MPL-2.0",    "mpl-2.0")
    ]

newtype License = License { unLicense :: Text }
    deriving (IsString, Eq, Ord, Show)

instance FromJSON License where
    parseJSON = withObject "License" $ \o -> License <$> o .: "body"

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
