{-# LANGUAGE QuasiQuotes #-}

module Summoner.Source
       ( Source (..)
       , sourceT
       , fetchSource
       ) where

import Control.Arrow ((>>>))
import Control.Exception (catch)
import NeatInterpolation (text)
import System.Process (readProcess)
import Toml (Key, TomlBiMap, TomlBiMapError (..), TomlCodec)

import Summoner.Ansi (errorMessage, infoMessage)

import qualified Toml


-- | Type of the source resource.
data Source
    -- | URL link to the source file.
    = Url !Text
    -- | File path to the local source file.
    | File !FilePath
    -- | Link to external file.
    | Link !Text
    deriving stock (Show, Eq)

showSource :: Source -> Text
showSource = \case
    Url _  -> "Url"
    File _ -> "File"
    Link _ -> "Link"

matchUrl :: Source -> Either TomlBiMapError Text
matchUrl (Url url) = Right url
matchUrl e         = Left $ WrongConstructor "Url" $ showSource e

matchFile :: Source -> Either TomlBiMapError FilePath
matchFile (File file) = Right file
matchFile e           = Left $ WrongConstructor "File" $ showSource e

matchLink :: Source -> Either TomlBiMapError Text
matchLink (Link link) = Right link
matchLink e           = Left $ WrongConstructor "Link" $ showSource e

sourceT :: Key -> TomlCodec Source
sourceT nm = Toml.match (_Url  >>> Toml._Text)   (nm <> "url")
         <|> Toml.match (_File >>> Toml._String) (nm <> "file")
         <|> Toml.match (_Link >>> Toml._Text)   (nm <> "link")
  where
    _Url :: TomlBiMap Source Text
    _Url = Toml.prism Url matchUrl

    _File :: TomlBiMap Source FilePath
    _File = Toml.prism File matchFile

    _Link :: TomlBiMap Source Text
    _Link = Toml.prism Link matchLink

fetchSource :: Bool -> Source -> IO (Maybe Text)
fetchSource isOffline = \case
    File path -> catch (Just <$> readFileText path) (fileError path)
    Url url   -> if isOffline
        then Nothing <$ infoMessage ("Ignoring fetching from URL in offline mode from source: " <> url)
        else fetchUrl url `catch` urlError url
    Link link -> putLink link
  where
    fileError :: FilePath -> SomeException -> IO (Maybe Text)
    fileError path _ = errorMessage ("Couldn't read file: " <> toText path)
                    >> pure Nothing

    urlError :: Text -> SomeException -> IO (Maybe Text)
    urlError url _ = errorMessage ("Couldn't get to link: " <> url)
                  >> pure Nothing

    fetchUrl :: Text -> IO (Maybe Text)
    fetchUrl url = Just . toText <$> readProcess "curl" [toString url] ""

    putLink :: Text -> IO (Maybe Text)
    putLink link = pure $ Just [text|See full content of the file [here]($link)|]
