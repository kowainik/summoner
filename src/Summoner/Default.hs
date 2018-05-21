-- | This module contains some default values to use.

module Summoner.Default
       ( defaultOwner
       , defaultName
       , defaultEmail
       , defaultLicense
       , defaultGHC
       , currentYear
       , endLine
       ) where

import Data.Text (Text)
import Data.Time (getCurrentTime, toGregorian, utctDay)

import Summoner.License (License)

import qualified Data.Text as T

----------------------------------------------------------------------------
-- Default Settings
----------------------------------------------------------------------------

defaultOwner :: Text
defaultOwner = "kowainik"

defaultName :: Text
defaultName = "Kowainik"

defaultEmail :: Text
defaultEmail = "xrom.xkov@gmail.com"

defaultLicense :: License
defaultLicense = "MIT"

defaultGHC :: Text
defaultGHC = "8.2.2"

currentYear :: IO Text
currentYear = do
    now <- getCurrentTime
    let (year, _, _) = toGregorian $ utctDay now
    pure $ T.pack $ show year

endLine :: Text
endLine = "\n"
