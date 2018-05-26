{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This module contains various function to work with commands.
module Summoner.Process
       ( deleteFile
       ) where

import Control.Exception (displayException)
import System.Directory (removeFile, setCurrentDirectory)
import System.Process (callProcess)

import Summoner.Ansi (errorMessage)

----------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------

-- This is needed to be able to call commands by writing strings.
instance (a ~ Text, b ~ ()) => IsString ([a] -> IO b) where
    fromString "cd" [arg] = setCurrentDirectory $ toString arg
    fromString cmd args   = callProcess cmd (map toString args)

-- Delete file, but just print a message if delete fails and continue instead of raising an error.
deleteFile :: FilePath -> IO  ()
deleteFile file = removeFile file `catch` printError
  where
    printError :: SomeException -> IO ()
    printError e = errorMessage $ "Could not delete file '"
        <> toText file <> "'. " <> toText  (displayException e)
