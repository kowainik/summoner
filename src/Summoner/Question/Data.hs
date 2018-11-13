{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Summoner.Question.Data
  ( YesNoPrompt(..)
  , mkYesNoPrompt
  , mkDefaultYesNoPrompt
  ) where

import Data.Text (Text)

data YesNoPrompt = YesNoPrompt
  { yesno_target :: Text
  , yesno_prompt :: Text
  }

-- | Build a prompt
--
-- For example,
--
-- @
-- YesNoPrompt
--   { yesno_target = "Cabal"
--   , yesno_prompt = "Do you want to add a cabal integration?"}
-- @
-- will generate a following prompt message to the user
--
-- @
-- Do you want to add a cabal integration? [y]/n
--  -> y
-- [Cabal] will be added to the project
-- @
mkYesNoPrompt ::
     Text -- ^ Prompt Text
  -> Text -- ^ Target Name
  -> YesNoPrompt
mkYesNoPrompt yesno_target yesno_prompt = YesNoPrompt {..}


-- | Build a prompt with the TARGET name only
-- It will generate a simple default prompt such that
--
-- @
-- Add TARGET? [y]/n
-- @
--
mkDefaultYesNoPrompt ::
     Text -- ^ Target name
  -> YesNoPrompt
mkDefaultYesNoPrompt target = mkYesNoPrompt target $ "Add " <> target <> "?"
