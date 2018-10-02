{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This module contains various function to work with commands.
module Summoner.Process
       ( deleteFile
       ) where

import Control.Exception (catch)
import System.Directory (removeFile)
import System.Process (callCommand, showCommandForUser)

import Summoner.Ansi (errorMessage)

----------------------------------------------------------------------------
-- Commands
----------------------------------------------------------------------------

-- This is needed to be able to call commands by writing strings.
instance (a ~ Text, b ~ ()) => IsString ([a] -> IO b) where
    fromString cmd args = do
        let cmdStr = showCommandForUser cmd (map toString args)
        putStrLn $ "⚙  " ++ cmdStr
        callCommand cmdStr

-- Delete file, but just print a message if delete fails and continue instead of raising an error.
deleteFile :: FilePath -> IO  ()
deleteFile file = removeFile file `catch` printError
  where
    printError :: SomeException -> IO ()
    printError e = errorMessage $ "Could not delete file '"
        <> toText file <> "'. " <> toText  (displayException e)
