{- |
Module                  : Summoner.Source
Copyright               : (c) 2017-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

This module contains the 'Source' data that describes how to fetch custom files.
-}

module Summoner.Source
       ( Source (..)
       , sourceCodec
       , fetchSources
       , fetchSource
       ) where

import Colourista (errorMessage, infoMessage)
import Control.Exception (catch)
import System.Process (readProcess)
import Toml (TomlBiMapError (..), TomlCodec)

import Summoner.Mode (ConnectMode (..), isOffline)
import Summoner.Tree (TreeFs, pathToTree)

import qualified Data.Map.Strict as Map
import qualified Toml


-- | Type of the source resource.
data Source
    {- | URL link to the source file. Such files will be downloaded by URL. But
    they are ingored in the @offline@ mode.
    -}
    = Url !Text

    {- | File path to the local source file.
    -}
    | Local !FilePath

    {- | Raw file text content.
    -}
    | Raw !Text
    deriving stock (Show, Eq)

showSource :: Source -> Text
showSource = \case
    Url _ -> "Url"
    Local _ -> "Local"
    Raw _ -> "Raw"

-- TODO: return Maybe
matchUrl :: Source -> Either TomlBiMapError Text
matchUrl (Url url) = Right url
matchUrl e         = Left $ WrongConstructor "Url" $ showSource e

-- TODO: return Maybe
matchLocal :: Source -> Either TomlBiMapError FilePath
matchLocal (Local file) = Right file
matchLocal e            = Left $ WrongConstructor "Local" $ showSource e

-- TODO: return Maybe
matchRaw :: Source -> Either TomlBiMapError Text
matchRaw (Raw raw) = Right raw
matchRaw e         = Left $ WrongConstructor "Raw" $ showSource e

{- | This 'TomlCodec' is used in the @files@ field of config. It decodes
corresponding constructor from the top-level key.
-}
sourceCodec :: TomlCodec Source
sourceCodec = asum
    [ Toml.dimatch (rightToMaybe . matchUrl) Url (Toml.text "url")
    , Toml.dimatch (rightToMaybe . matchLocal) Local (Toml.string "local")
    , Toml.dimatch (rightToMaybe . matchRaw) Raw (Toml.text "raw")
    ]

{- | This function fetches contents of extra file sources.
-}
fetchSources :: ConnectMode -> Map FilePath Source -> IO [TreeFs]
fetchSources connectMode = mapMaybeM sourceToTree . Map.toList
  where
    sourceToTree :: (FilePath, Source) -> IO (Maybe TreeFs)
    sourceToTree (path, source) = do
        infoMessage $ "Fetching content of the extra file: " <> toText path
        fetchSource connectMode source >>= \case
            Nothing -> Nothing <$ errorMessage ("Error fetching: " <> toText path)
            Just content -> pure $ Just $ pathToTree path content

{- | Fetches content of the given extra file source.
Doesn't fetch 'Url' if the 'ConnectMode' is 'Offline'.
-}
fetchSource :: ConnectMode -> Source -> IO (Maybe Text)
fetchSource connectMode = \case
    Local path -> catch (Just <$> readFileText path) (localError path)
    Url url -> if isOffline connectMode
        then Nothing <$ infoMessage ("Ignoring fetching from URL in offline mode from source: " <> url)
        else fetchUrl url `catch` urlError url
    Raw raw -> pure $ Just raw
  where
    localError :: FilePath -> SomeException -> IO (Maybe Text)
    localError path err = do
        errorMessage ("Couldn't read file: " <> toText path)
        errorMessage $ toText $ displayException err
        pure Nothing

    urlError :: Text -> SomeException -> IO (Maybe Text)
    urlError url err = do
        errorMessage ("Couldn't get to link: " <> url)
        errorMessage $ toText $ displayException err
        pure Nothing

    fetchUrl :: Text -> IO (Maybe Text)
    fetchUrl url = Just . toText <$> readProcess "curl" [toString url, "--silent", "--fail"] ""
