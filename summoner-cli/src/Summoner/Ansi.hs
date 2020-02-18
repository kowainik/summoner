{- |
Copyright: (c) 2017-2019 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

This module contains functions for colorful printing into terminal.
-}

module Summoner.Ansi
       ( boldDefault
       , putStrFlush
       , prompt
       , successMessage
       , warningMessage
       , errorMessage
       , infoMessage
       , skipMessage
       ) where

import Colourista (blue, bold, formatWith)
import System.IO (hFlush)

import qualified Colourista
import qualified Data.Text as T


-- | Explicit flush ensures prompt messages are in the correct order on all systems.
putStrFlush :: Text -> IO ()
putStrFlush msg = do
    putText msg
    hFlush stdout

{- | Read 'Text' from standard input after arrow prompt.
-}
prompt :: IO Text
prompt = do
    putStrFlush $ formatWith [blue] "  ->   "
    T.strip <$> getLine

boldDefault :: Text -> Text
boldDefault message = formatWith [bold] $ " [" <> message <> "]"

errorMessage, warningMessage, successMessage, infoMessage, skipMessage :: Text -> IO ()
errorMessage   = Colourista.errorMessage   . indent
warningMessage = Colourista.warningMessage . indent
successMessage = Colourista.successMessage . indent
infoMessage    = Colourista.infoMessage    . indent
skipMessage    = Colourista.skipMessage    . indent

-- | Add 2 spaces in front.
indent :: Text -> Text
indent = ("  " <>)
