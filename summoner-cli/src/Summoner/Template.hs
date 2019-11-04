-- | This module contains functions for stack template creation.

module Summoner.Template
       ( createProjectTemplate
       ) where

import Summoner.Settings (Settings (..))
import Summoner.Template.Cabal (cabalFile)
import Summoner.Template.Doc (docFiles)
import Summoner.Template.GitHub (gitHubFiles)
import Summoner.Template.Haskell (haskellFiles)
import Summoner.Template.Stack (stackFiles)
import Summoner.Tree (TreeFs (..), insertTree)


-- | Creating tree structure of the project.
createProjectTemplate :: Settings -> TreeFs
createProjectTemplate settings@Settings{..} = Dir (toString settingsRepo)
    $ foldr insertTree settingsFiles
    $ concat
        [ cabal
        , stack
        , haskell
        , docs
        , gitHub
        ]
  where
    cabal, stack :: [TreeFs]
    cabal = [cabalFile settings]
    stack = memptyIfFalse settingsStack $ stackFiles settings

    haskell, docs, gitHub :: [TreeFs]
    haskell = haskellFiles settings
    docs    = docFiles settings
    gitHub  = gitHubFiles settings
