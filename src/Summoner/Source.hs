{-# LANGUAGE QuasiQuotes     #-}

module Summoner.Source
       ( Source (..)
       , sourceT
       , fetchSource
       ) where

import           Control.Arrow ((>>>))
import           Control.Exception (catch)
import           NeatInterpolation (text)
import           System.Process (readProcess)
import           Toml (BiMap, BiToml, Key)

import           Summoner.Ansi (errorMessage)

import qualified Toml

data Source
    -- | URL link to the source file.
    = Url Text
    -- | File path to the local source file.
    | File FilePath
    -- | Link to external file.
    | Link Text
    deriving (Show, Eq)

sourceT :: Key -> BiToml Source
sourceT nm = Toml.match (Toml._Text   >>> _Url)  (nm <> "url")
         <|> Toml.match (Toml._String >>> _File) (nm <> "file")
         <|> Toml.match (Toml._Text >>> _Link) (nm <> "link")
  where
    _Url :: BiMap Text Source
    _Url = Toml.invert $ Toml.prism (source Just (const Nothing) (const Nothing)) Url

    _File :: BiMap FilePath Source
    _File = Toml.invert $ Toml.prism (source (const Nothing) Just (const Nothing)) File

    _Link :: BiMap Text Source
    _Link = Toml.invert $ Toml.prism (source (const Nothing) (const Nothing) Just) Link

    source :: (Text -> c) -> (FilePath -> c) -> (Text -> c) -> Source -> c
    source f _ _ (Url x)  = f x
    source _ f _ (File x) = f x
    source _ _ f (Link x) = f x

fetchSource :: Source -> IO (Maybe Text)
fetchSource = \case
    File path -> catch (Just <$> readFileText path) (fileError path)
    Url url -> catch (fetchUrl url) (urlError url)
    Link link -> catch (putLink link) (urlError link)
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
