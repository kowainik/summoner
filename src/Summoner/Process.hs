{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This module contains various function to work with commands.
module Summoner.Process
       ( deleteFile
       ) where

import Control.Exception (SomeException, catch, displayException)
import Data.Semigroup ((<>))
import Data.String (IsString (..))
import Data.Text (Text)
import System.Directory (removeFile, setCurrentDirectory)
import System.Process (callProcess)

import Summoner.Ansi (errorMessage)

import qualified Data.Text as T

----------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------

-- This is needed to be able to call commands by writing strings.
instance (a ~ Text, b ~ ()) => IsString ([a] -> IO b) where
    fromString "cd" [arg] = setCurrentDirectory $ T.unpack arg
    fromString cmd args   = callProcess cmd (map T.unpack args)

-- Delete file, but just print a message if delete fails and continue instead of raising an error.
deleteFile :: FilePath -> IO  ()
deleteFile file = catch (removeFile file) printError
  where
    printError :: SomeException -> IO ()
    printError e = errorMessage $ "Could not delete file '"
        <> T.pack file <> "'. " <> T.pack  (displayException e)
