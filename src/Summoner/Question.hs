{-# LANGUAGE MultiWayIf #-}

-- | This module contains function to proper questioning in terminal.

module Summoner.Question
       ( printQuestion
       , choose
       , query
       , queryDef
       , checkUniqueName
       ) where

import Data.Semigroup ((<>))
import Data.Text (Text)
import System.Directory (doesPathExist, getCurrentDirectory)
import System.FilePath ((</>))

import Summoner.Ansi (boldDefault, errorMessage, prompt, putStrFlush)

import qualified Data.Text as T
import qualified Data.Text.IO as T


----------------------------------------------------------------------------
-- IO Questioning
----------------------------------------------------------------------------

printQuestion :: Text -> [Text] -> IO ()
printQuestion question (def:rest) = do
    let restSlash = T.intercalate "/" rest
    putStrFlush question
    boldDefault def
    T.putStrLn $ "/" <> restSlash
printQuestion question [] = T.putStrLn question

choose :: Text -> [Text] -> IO Text
choose question choices = do
    printQuestion question choices
    answer <- prompt
    if | T.null answer -> pure (head choices)
       | answer `elem` choices -> pure answer
       | otherwise -> do
           errorMessage "This wasn't a valid choice."
           choose question choices

query :: Text -> IO Text
query question = do
    T.putStrLn question
    answer <- prompt
    if | T.null answer -> do
           errorMessage "An answer is required."
           query question
       | otherwise -> pure answer

queryDef :: Text -> Text -> IO Text
queryDef question defAnswer = do
    putStrFlush question
    boldDefault defAnswer
    T.putStrLn ""
    answer <- prompt
    if | T.null answer -> pure defAnswer
       | otherwise     -> pure answer

checkUniqueName :: Text -> IO Text
checkUniqueName nm = do
    curPath <- getCurrentDirectory
    exist   <- doesPathExist $ curPath </> T.unpack nm
    if exist then do
        errorMessage "Project with this name is already exist. Please choose another one"
        newNm <- query "Project name: "
        checkUniqueName newNm
    else
        pure nm
