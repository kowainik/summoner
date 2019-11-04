{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- QUESTION: should we rename this module to Summoner.Shell?
-- | This module contains various function to work with commands.
module Summoner.Process
       ( createFileWithParents
       , deleteFile
       ) where

import Control.Exception (catch)
import System.Directory (createDirectoryIfMissing, removeFile)
import System.FilePath (takeDirectory)

import Summoner.Ansi (errorMessage)


-- | Delete file, but just print a message if delete fails and continue instead of raising an error.
deleteFile :: FilePath -> IO  ()
deleteFile file = removeFile file `catch` printError
  where
    printError :: SomeException -> IO ()
    printError e = errorMessage $ "Could not delete file '"
        <> toText file <> "'. " <> toText  (displayException e)

{- | This function creates all parents of a given file and writes content to the
file by path. If given a path like @foo/bar/baz/quux.txt@ it will create all
directories @foo/@, @foo/bar/@ and @foo/bar/baz/@.
-}
createFileWithParents :: FilePath -> Text -> IO ()
createFileWithParents path content = do
    let directory = takeDirectory path
    createDirectoryIfMissing True directory
    writeFileText path content
