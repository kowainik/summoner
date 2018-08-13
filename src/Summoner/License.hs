module Summoner.License
       ( LicenseName(..)
       , License(..)
       , customizeLicense
       , githubLicenseQueryNames
       , parseLicenseName
       ) where

import Relude
import Relude.Extra.Enum (inverseMap)

import Data.Aeson (FromJSON (..), withObject, (.:))

import qualified Data.Text as T
import qualified Text.Read as TR
import qualified Text.Show as TS

----------------------------------------------------------------------------
-- License
----------------------------------------------------------------------------

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
    deriving (Eq, Ord, Enum, Bounded, Generic)

instance Show LicenseName where
    show MIT      = "MIT"
    show BSD2     = "BSD2"
    show BSD3     = "BSD3"
    show GPL2     = "GPL-2"
    show GPL3     = "GPL-3"
    show LGPL21   = "LGPL-2.1"
    show LGPL3    = "LGPL-3"
    show AGPL3    = "AGPL-3"
    show Apache20 = "Apache-2.0"
    show MPL20    = "MPL-2.0"

instance Read LicenseName where
    readPrec = TR.parens
        (do TR.Ident s <- TR.lexP
            let mLicenseName = parseLicenseName (T.pack s)
            case mLicenseName of
                Just l  -> return l
                Nothing -> TR.pfail
        )

newtype License = License { unLicense :: Text }
    deriving (IsString, Show, Generic)

instance FromJSON License where
    parseJSON = withObject "License" $ \o -> License <$> o .: "body"

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

parseLicenseName :: Text -> Maybe LicenseName
parseLicenseName = inverseMap show

customizeLicense :: LicenseName -> Text -> Text -> Text -> Text
customizeLicense l t nm year
    | l `elem` [MIT, BSD2, BSD3] = updateLicenseText
    | otherwise                  = t
  where
    updateLicenseText =
        let (beforeY, withY) = T.span (/= '[') t
            afterY           = T.tail $ T.dropWhile (/= ']') withY
            (beforeN, withN) = T.span (/= '[') afterY
            afterN           = T.tail $ T.dropWhile (/= ']') withN
        in  beforeY <> year <> beforeN <> nm <> afterN
