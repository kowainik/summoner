{- |
Copyright: (c) 2017-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module contains the 'Source' data that describes how to fetch custom files.
-}

module Summoner.Source
       ( Source (..)
       , sourceT
       , sourceCodec
       , fetchSource
       ) where

import Control.Arrow ((>>>))
import Control.Exception (catch)
import System.Process (readProcess)
import Toml (Key, TomlBiMap, TomlBiMapError (..), TomlCodec)

import Summoner.Ansi (errorMessage, infoMessage)

import qualified Toml


-- | Type of the source resource.
data Source
    {- | URL link to the source file. Such files will be downloaded by URL. But
    they are ingored in the @offline@ mode.
    -}
    = Url !Text

    {- | File path to the local source file.
    -}
    | File !FilePath

    {- | Raw file text content.
    -}
    | Raw !Text
    deriving stock (Show, Eq)

showSource :: Source -> Text
showSource = \case
    Url  _ -> "Url"
    File _ -> "File"
    Raw  _ -> "Raw"

-- TODO: return Maybe
matchUrl :: Source -> Either TomlBiMapError Text
matchUrl (Url url) = Right url
matchUrl e         = Left $ WrongConstructor "Url" $ showSource e

-- TODO: return Maybe
matchFile :: Source -> Either TomlBiMapError FilePath
matchFile (File file) = Right file
matchFile e           = Left $ WrongConstructor "File" $ showSource e

-- TODO: return Maybe
matchRaw :: Source -> Either TomlBiMapError Text
matchRaw (Raw raw) = Right raw
matchRaw e         = Left $ WrongConstructor "Raw" $ showSource e

-- DEPRECATED: To be removed in 2.0
sourceT :: Key -> TomlCodec Source
sourceT nm = Toml.match (_Url  >>> Toml._Text)   (nm <> "url")
         <|> Toml.match (_File >>> Toml._String) (nm <> "file")
         <|> Toml.match (_Raw  >>> Toml._Text)   (nm <> "raw")
  where
    _Url :: TomlBiMap Source Text
    _Url = Toml.prism Url matchUrl

    _File :: TomlBiMap Source FilePath
    _File = Toml.prism File matchFile

    _Raw :: TomlBiMap Source Text
    _Raw = Toml.prism Raw matchRaw

{- | This 'TomlCodec' is used in the @files@ field of config. It decodes
corresponding constructor from the top-level key.
-}
sourceCodec :: TomlCodec Source
sourceCodec = asum
    [ Toml.dimatch (rightToMaybe . matchUrl) Url (Toml.text "url")
    , Toml.dimatch (rightToMaybe . matchFile) File (Toml.string "file")
    , Toml.dimatch (rightToMaybe . matchRaw) Raw (Toml.text "raw")
    ]

fetchSource :: Bool -> Source -> IO (Maybe Text)
fetchSource isOffline = \case
    File path -> catch (Just <$> readFileText path) (fileError path)
    Url url -> if isOffline
        then Nothing <$ infoMessage ("Ignoring fetching from URL in offline mode from source: " <> url)
        else fetchUrl url `catch` urlError url
    Raw raw -> pure $ Just raw
  where
    fileError :: FilePath -> SomeException -> IO (Maybe Text)
    fileError path _ = errorMessage ("Couldn't read file: " <> toText path)
                    >> pure Nothing

    urlError :: Text -> SomeException -> IO (Maybe Text)
    urlError url _ = errorMessage ("Couldn't get to link: " <> url)
                  >> pure Nothing

    fetchUrl :: Text -> IO (Maybe Text)
    fetchUrl url = Just . toText <$> readProcess "curl" [toString url] ""
