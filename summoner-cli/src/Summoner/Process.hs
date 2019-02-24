{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This module contains various function to work with commands.
module Summoner.Process
       ( deleteFile
       ) where

import Control.Exception (catch)
import System.Directory (removeFile)

import Summoner.Ansi (errorMessage)


-- | Delete file, but just print a message if delete fails and continue instead of raising an error.
deleteFile :: FilePath -> IO  ()
deleteFile file = removeFile file `catch` printError
  where
    printError :: SomeException -> IO ()
    printError e = errorMessage $ "Could not delete file '"
        <> toText file <> "'. " <> toText  (displayException e)
