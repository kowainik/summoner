{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module contains function to proper questioning in terminal.

module Summoner.Question
       ( printQuestion
       , choose
       , chooseYesNo
       , chooseYesNoBool
       , query
       , queryDef
       , queryManyRepeatOnFail
       , checkUniqueName

       , targetMessageWithText

       , trueMessage
       , falseMessage
       ) where

import Relude

import System.Directory (doesPathExist, getCurrentDirectory)
import System.FilePath ((</>))

import Summoner.Ansi (Color (..), beautyPrint, bold, boldDefault, errorMessage, italic, prompt,
                      putStrFlush, setColor, warningMessage)
import Summoner.ProjectData (Answer (..), yesOrNo)
import Summoner.Text (headToUpper, intercalateMap)

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Relude.Unsafe as Unsafe

----------------------------------------------------------------------------
-- IO Questioning
----------------------------------------------------------------------------

printQuestion :: Text -> [Text] -> IO ()
printQuestion question (def:rest) = do
    let restSlash = T.intercalate "/" rest
    putStrFlush question
    boldDefault def
    putTextLn $ "/" <> restSlash
printQuestion question [] = T.putStrLn question

choose :: (Show a, IsString a) => Text -> [a] -> IO a
choose question choices = do
    let showChoices = map show choices
    printQuestion question showChoices
    answer <- prompt
    if | T.null answer -> pure (Unsafe.head choices)
       | answer `elem` showChoices -> pure $ fromString $ T.unpack answer
       | otherwise -> do
           errorMessage "This wasn't a valid choice."
           choose question choices

chooseYesNo :: Text -- ^ target
            -> IO a -- ^ action for 'Y' answer
            -> IO a -- ^ action for 'N' answer
            -> IO a
chooseYesNo target yesDo noDo = do
    printQuestion ("Add " <> target <> "?") ["y", "n"]
    answer <- yesOrNo <$> prompt
    case answer of
        Nothing -> do
           errorMessage "This wasn't a valid choice."
           chooseYesNo target yesDo noDo
        Just Y -> trueMessage target >> yesDo
        Just N -> falseMessage target >> noDo

chooseYesNoBool :: Text -> IO Bool
chooseYesNoBool target = chooseYesNo target (pure True) (pure False)

targetMessage :: Bool -> Text -> IO Bool
targetMessage result target = targetMessageWithText result target "added to the project"

targetMessageWithText :: Bool -> Text -> Text -> IO Bool
targetMessageWithText result target text = do
    let (color, actionResult) = if result
          then (Green, " will be " <> text)
          else (Cyan,  " won't be" <> text)

    beautyPrint [italic, bold, setColor color] $ "  " <> headToUpper target
    beautyPrint [setColor color] actionResult
    putTextLn ""

    pure result

trueMessage, falseMessage :: Text -> IO Bool
trueMessage  = targetMessage True
falseMessage = targetMessage False

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
    putTextLn ""
    answer <- prompt
    if | T.null answer -> pure defAnswer
       | otherwise     -> pure answer

queryManyRepeatOnFail :: forall a . (Text -> Maybe a) -> IO [a]
queryManyRepeatOnFail parser = promptLoop
  where
    promptLoop :: IO [a]
    promptLoop = do
        answer <- prompt
        let answers = map (id &&& parser) $ words answer  -- converts [Text] into [(Text, Maybe a)]
        case partitionPairs answers of
            Left unparsed -> do
                -- TODO: create intercalateMap function
                errorMessage $ "Unable to parse the following items: " <> intercalateMap " " quote unparsed
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
    exist   <- doesPathExist $ curPath </> toString nm
    if exist then do
        warningMessage "Project with this name is already exist. Please choose another one"
        newNm <- query "Project name: "
        checkUniqueName newNm
    else
        pure nm
