{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains function to proper questioning in terminal.

module Summoner.Question
       ( printQuestion
       , choose
       , query
       , queryDef
       , queryManyRepeatOnFail
       , checkUniqueName
       ) where

import Control.Arrow ((&&&))
import Data.Semigroup ((<>))
import Data.Text (Text)
import System.Directory (doesPathExist, getCurrentDirectory)
import System.FilePath ((</>))

import Summoner.Ansi (boldDefault, errorMessage, prompt, putStrFlush, warningMessage)

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

queryManyRepeatOnFail :: forall a . (Text -> Maybe a) -> IO [a]
queryManyRepeatOnFail parser = do
    promptLoop
  where
    promptLoop :: IO [a]
    promptLoop = do
        answer <- prompt
        let answers = map (id &&& parser) $ T.words answer  -- converts [Text] into [(Text, Maybe a)]
        case partitionPairs answers of
            Left unparsed -> do
                -- TODO: create intercalateMap function
                errorMessage $ "Unable to parse the following items: " <> T.intercalate " " (map quote unparsed)
                promptLoop
            Right results -> pure results

    -- puts only those @c@ into Left list where snd is Nothing;
    -- returns Left if at least one second element is Nothing
    partitionPairs :: forall x y . [(x, Maybe y)] -> Either [x] [y]
    partitionPairs [] = Right []
    partitionPairs ((x, my):xs) = case my of
        Just y -> (y:) <$> partitionPairs xs
        Nothing -> case partitionPairs xs of
            Left fails -> Left (x : fails)
            Right _    -> Left [x]

    quote :: Text -> Text
    quote t = "'" <> t <> "'"

checkUniqueName :: Text -> IO Text
checkUniqueName nm = do
    curPath <- getCurrentDirectory
    exist   <- doesPathExist $ curPath </> T.unpack nm
    if exist then do
        warningMessage "Project with this name is already exist. Please choose another one"
        newNm <- query "Project name: "
        checkUniqueName newNm
    else
        pure nm
