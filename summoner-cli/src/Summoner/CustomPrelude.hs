module Summoner.CustomPrelude
       ( CustomPrelude (..)
       , customPreludeT
       ) where

import Toml (Key, TomlCodec, (.=))

import Summoner.Text (moduleNameValid, packageNameValid)

import qualified Toml


data CustomPrelude = CustomPrelude
    { cpPackage :: Text
    , cpModule  :: Text
    } deriving (Show, Eq)

customPreludeT :: TomlCodec CustomPrelude
customPreludeT = CustomPrelude
    <$> textWithBool packageNameValid "package" .= cpPackage
    <*> textWithBool moduleNameValid  "module"  .= cpModule

-- | Codec for text values.
textWithBool :: (Text -> Bool) -> Key -> TomlCodec Text
textWithBool p = Toml.textBy id validateText
  where
    validateText :: Text -> Either Text Text
    validateText s =
        if p s
        then Right s
        else Left "Given Text doesn't pass the validation"
{-# INLINE textWithBool #-}
