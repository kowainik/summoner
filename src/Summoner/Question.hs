{-# LANGUAGE MultiWayIf   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | This module contains function to proper questioning in terminal.

module Summoner.Question
       ( printQuestion
       , choose
       , chooseYesNo
       , chooseYesNoBool
       , YesNoPrompt(..)
       , mkDefaultYesNoPrompt
       , query
       , queryDef
       , queryManyRepeatOnFail
       , checkUniqueName

       , targetMessageWithText

       , trueMessage
       , falseMessage
       ) where

import System.Directory (doesPathExist, getCurrentDirectory)
import System.FilePath ((</>))

import Summoner.Ansi (Color (..), beautyPrint, bold, boldDefault, errorMessage, italic, prompt,
                      putStrFlush, setColor, warningMessage)
import Summoner.Text (headToUpper, intercalateMap)

import qualified Data.Text as T
import qualified Relude.Unsafe as Unsafe

----------------------------------------------------------------------------
-- Yes/No
----------------------------------------------------------------------------

-- | Build a prompt
--
-- For example,
--
-- @
-- YesNoPrompt
--   { yesNoTarget = "Cabal"
--   , yesNoPrompt = "Do you want to add a cabal integration?"}
-- @
-- will generate a following prompt message to the user
--
-- @
-- Do you want to add a cabal integration? [y]/n
--  -> y
-- [Cabal] will be added to the project
-- @
data YesNoPrompt = YesNoPrompt
  { yesNoTarget :: Text -- ^ target (e.g., __TARGET will be added to the project__)
  , yesNoPrompt :: Text -- ^ prompt (e.g., __PROMPT [y]/n__)
  }

-- | Build a prompt with the TARGET name only
--
-- It will generate a simple default prompt such that
--
-- @
-- Add TARGET? [y]/n
-- @
--
mkDefaultYesNoPrompt ::
     Text -- ^ target name
  -> YesNoPrompt
mkDefaultYesNoPrompt target = YesNoPrompt target ("Add " <> target <> "?")

-- | Represents a user's answer
data Answer = Y | N

-- | Parse an answer to 'Answer'
yesOrNo :: Text -> Maybe Answer
yesOrNo (T.toLower -> answer )
    | T.null answer = Just Y
    | answer `elem` ["yes", "y", "ys"] = Just Y
    | answer `elem` ["no", "n"]  = Just N
    | otherwise = Nothing

----------------------------------------------------------------------------
-- IO Questioning
----------------------------------------------------------------------------

printQuestion :: Text -> [Text] -> IO ()
printQuestion question (def:rest) = do
    let restSlash = T.intercalate "/" rest
    putStrFlush question
    boldDefault def
    putTextLn $ "/" <> restSlash
printQuestion question [] = putTextLn question

choose :: Show a => (Text -> Maybe a) -> Text -> [a] -> IO a
choose parser question choices = do
    let showChoices = map show choices
    printQuestion question showChoices
    answer <- prompt
    if T.null answer
        then pure (Unsafe.head choices)
        else whenNothing (parser answer)
                (errorMessage "This wasn't a valid choice." >> choose parser question choices)

chooseYesNo :: YesNoPrompt -- ^ Target and Prompt
            -> IO a -- ^ action for 'Y' answer
            -> IO a -- ^ action for 'N' answer
            -> IO a
chooseYesNo p@YesNoPrompt {..} yesDo noDo = do
    printQuestion yesNoPrompt ["y", "n"]
    answer <- yesOrNo <$> prompt
    case answer of
        Nothing -> do
           errorMessage "This wasn't a valid choice."
           chooseYesNo p yesDo noDo
        Just Y -> trueMessage yesNoTarget >> yesDo
        Just N -> falseMessage yesNoTarget >> noDo

chooseYesNoBool :: YesNoPrompt -> IO Bool
chooseYesNoBool ynPrompt = chooseYesNo ynPrompt (pure True) (pure False)

targetMessage :: Bool -> Text -> IO Bool
targetMessage result target = targetMessageWithText result target "added to the project"

targetMessageWithText :: Bool -> Text -> Text -> IO Bool
targetMessageWithText result target text = do
    let (color, actionResult) = if result
          then (Green, " will be "  <> text)
          else (Cyan,  " won't be " <> text)

    beautyPrint [italic, bold, setColor color] $ "  " <> headToUpper target
    beautyPrint [setColor color] actionResult
    putTextLn ""

    pure result

trueMessage, falseMessage :: Text -> IO Bool
trueMessage  = targetMessage True
falseMessage = targetMessage False

query :: Text -> IO Text
query question = do
    putTextLn question
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
