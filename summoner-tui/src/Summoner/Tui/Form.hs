{- |
Module                  : Summoner.Tui.Form
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

Form layout and form fields data types.
-}

module Summoner.Tui.Form
       ( SummonForm (..)
       , KitForm
       , mkForm
       , getCurrentFocus
       , isActive
       , recreateForm
       ) where

import Brick (Padding (Max), Widget, hBox, padRight, str, txt, vBox, vLimit)
import Brick.Focus (focusGetCurrent)
import Brick.Forms (Form, editField, editTextField, formFocus, formState, listField, newForm,
                    setFieldConcat, setFormConcat, setFormFocus, (@@=))
import Lens.Micro ((^.))

import Summoner.Default (defaultGHC)
import Summoner.GhcVer (parseGhcVer, showGhcVer)
import Summoner.License (LicenseName)
import Summoner.Text (intercalateMap)
import Summoner.Tui.Field (activeCheckboxField, checkboxField, radioField, strField)
import Summoner.Tui.GroupBorder (groupBorder, (|>))
import Summoner.Tui.Kit
import Summoner.Tui.Widget (borderLabel, hArrange, label)

import qualified Brick.Widgets.Center as C
import qualified Data.Text as T


-- | Form that is used for @new@ command.
data SummonForm
    -- User
    = UserOwner
    | UserFullName
    | UserEmail

    -- Project
    | ProjectName
    | ProjectDesc
    | ProjectCat
    | ProjectLicense

    -- Build tools
    | CabalField
    | StackField

    -- Project Meta
    | Lib
    | Exe
    | Test
    | Bench
    | CustomPreludeName
    | CustomPreludeModule
    | Ghcs

      -- GitHub fields
    | GitHubEnable
    | GitHubDisable
    | GitHubNoUpload
    | GitHubPrivate
    | GitHubActions
    | GitHubTravis
    | GitHubAppVeyor
    deriving stock (Show, Eq, Ord, Enum, Bounded)

-- | Alias for type of the @summoner@ form.
type KitForm e = Form SummonKit e SummonForm

-- | Creates the input form from the given initial 'SummonKit'.
mkForm :: forall e . SummonKit -> KitForm e
mkForm sk = setFormConcat arrangeColumns $ newForm
    ( groupBorder "User"
        [ 2 |> label "Owner     " @@= editTextField (user . owner) UserOwner (Just 1)
        , 1 |> label "Full name " @@= editTextField (user . fullName) UserFullName (Just 1)
        , 2 |> label "Email     " @@= editTextField (user . email) UserEmail (Just 1)
        ]
   ++ groupBorder "Project"
        [ 2 |> label "Name        " @@= editTextField (project . repo) ProjectName (Just 1)
        , 3 |> label "Description " @@= editTextField (project . desc) ProjectDesc (Just 2)
        , 2 |> label "Category    " @@= editTextField (project . category) ProjectCat (Just 1)
        , 4 |> vLimit 3 . label "License " @@= listField (const (fromList $ universe @LicenseName))
              maybeLicense widgetList 1 ProjectLicense
        ]
   -- ++ groupBorder "Tools"
   --      [ 2 |> checkboxField cabal CabalField "Cabal"
   --      , 2 |> checkboxField stack StackField "Stack"
   --      ]
   ++   [ checkboxField cabal CabalField "Cabal"
        , checkboxField stack StackField "Stack"
        ]

   ++ groupBorder "Project Meta"
        [ 2 |> checkboxField (projectMeta . lib) Lib "Library"
        , 1 |> checkboxField (projectMeta . exe) Exe "Executable"
        , 1 |> checkboxField (projectMeta . test) Test "Tests"
        , 2 |> checkboxField (projectMeta . bench) Bench "Benchmarks"
        , 1 |> strField "Custom prelude"
        , 1 |> label "Name   " @@= editTextField (projectMeta . preludeName) CustomPreludeName (Just 1)
        , 2 |> label "Module " @@= editTextField (projectMeta . preludeModule) CustomPreludeModule (Just 1)
        , 2 |> label ("GHC versions: " <> toString (showGhcVer defaultGHC) <> " ") @@=
            editField
                (projectMeta . ghcs)
                Ghcs
                (Just 1)
                (intercalateMap " " showGhcVer)
                (traverse parseGhcVer . words . T.intercalate " ")
                (txt . T.intercalate "\n")
                id
        ]
   ++ groupBorder "GitHub"
        [ 2 |> setFieldConcat hArrange . radioField (gitHub . enabled)
            [ (True, GitHubEnable, "Enable")
            , (False, GitHubDisable, "Disable")
            ]
        , 1 |> activeCheckboxField (gitHub . noUpload) isActive GitHubNoUpload "No upload"
        , 1 |> activeCheckboxField (gitHub . private)  isActive GitHubPrivate  "Private"
        , 1 |> activeCheckboxField (gitHub . actions)  isActive GitHubActions  "GitHub Actions"
        , 1 |> activeCheckboxField (gitHub . travis)   isActive GitHubTravis   "Travis"
        , 2 |> activeCheckboxField (gitHub . appVeyor) isActive GitHubAppVeyor "AppVeyor"
        ]
    ) sk
  where
    widgetList :: Bool -> LicenseName -> Widget SummonForm
    widgetList p l = C.hCenter $ str $ if p then "[" ++ show l ++ "]" else show l

    arrangeColumns :: [Widget SummonForm] -> Widget SummonForm
    arrangeColumns widgets =
        let (column1, columns2) = splitAt 7 widgets in
        let (tools, column2) = splitAt 2 columns2 in
        hBox [ vBox $ column1 ++ [borderLabel "Tools" $ padRight Max (hArrange tools)]
             , vBox column2
             ]

-- | Returns 'True' when form field is active depending on the current state of 'SummonKit'.
isActive :: SummonKit -> SummonForm -> Bool
isActive kit = \case
    GitHubNoUpload -> isGitHubEnabled
    GitHubPrivate  -> isGitHubEnabled && isUploadEnabled
    GitHubActions  -> isGitHubEnabled && (kit ^. cabal)
    GitHubTravis   -> isGitHubEnabled
    GitHubAppVeyor -> isGitHubEnabled
    _nonGithub     -> True
  where
    isGitHubEnabled, isUploadEnabled :: Bool
    isGitHubEnabled = kit ^. gitHub . enabled
    isUploadEnabled = not $ kit ^. gitHub . noUpload

-- | Gets current focus of the form.
getCurrentFocus :: Form s e n -> Maybe n
getCurrentFocus = focusGetCurrent . formFocus

{- | Create form from scratch using its current state. This is needed to
activate/deactivate checkboxes. Should be done with care to preserve focus and
fields validation.
-}
recreateForm
    :: forall e .
       (KitForm e -> KitForm e)  -- ^ Validation function
    -> KitForm e  -- ^ Original form
    -> KitForm e  -- ^ New form
recreateForm validate kitForm = setFocus $ validate $ mkForm $ formState kitForm
  where
    setFocus :: KitForm e -> KitForm e
    setFocus = maybe id setFormFocus (getCurrentFocus kitForm)
