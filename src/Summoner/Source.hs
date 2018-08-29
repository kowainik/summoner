module Summoner.Source
       ( Source (..)
       , sourceT
       , fetchSource
       ) where

import Relude

import Control.Arrow ((>>>))
import Control.Exception (catch)
import System.Process (readProcess)
import Toml (BiMap, BiToml, Key)

import Summoner.Ansi (errorMessage)

import qualified Toml

data Source
    -- | URL link to the source file.
    = Url Text
    -- | File path to the local source file.
    | File FilePath
    deriving (Show, Eq)

sourceT :: Key -> BiToml Source
sourceT nm = Toml.match (Toml._Text   >>> _Url)  (nm <> "url")
         <|> Toml.match (Toml._String >>> _File) (nm <> "file")
  where
    _Url :: BiMap Text Source
    _Url = Toml.invert $ Toml.prism (source Just (const Nothing)) Url

    _File :: BiMap FilePath Source
    _File = Toml.invert $ Toml.prism (source (const Nothing) Just) File

    source :: (Text -> c) -> (FilePath -> c) -> Source -> c
    source f _ (Url x)  = f x
    source _ f (File x) = f x

fetchSource :: Source -> IO (Maybe Text)
fetchSource = \case
    File path -> catch (Just <$> readFile path) (fileError path)
    Url url -> catch (fetchUrl url) (urlError url)
  where
    fileError :: FilePath -> SomeException -> IO (Maybe Text)
    fileError path _ = errorMessage ("Couldn't read file: " <> toText path)
                    >> pure Nothing

    urlError :: Text -> SomeException -> IO (Maybe Text)
    urlError url _ = errorMessage ("Couldn't get to link: " <> url)
                  >> pure Nothing

    fetchUrl :: Text -> IO (Maybe Text)
    fetchUrl url = Just . toText <$> readProcess "curl" [toString url] ""
