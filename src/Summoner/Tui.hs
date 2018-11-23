{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE ViewPatterns #-}

{- | This is experimental module.

We are trying to make the TUI app for summoner, but it's WIP.
-}

module Summoner.Tui
       ( summonTui
       ) where

import Brick (App (..), AttrMap, BrickEvent (..), Padding (Max, Pad), Widget, attrMap, continue,
              customMain, hBox, halt, padRight, padTop, simpleApp, str, txt, vBox, vLimit, withAttr,
              (<+>))
import Brick.Focus (focusGetCurrent, focusRingCursor)
import Brick.Forms (Form, checkboxField, editField, editTextField, focusedFormInputAttr, formFocus,
                    formState, handleFormEvent, invalidFields, invalidFormInputAttr, listField,
                    newForm, radioField, renderForm, setFieldConcat, setFieldValid, setFormConcat,
                    (@@=))
import Brick.Types (ViewportType (Vertical))
import Lens.Micro ((.~), (^.))
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)

import Summoner.Ansi (errorMessage, infoMessage)
import Summoner.CLI (Command (..), NewOpts, ShowOpts (..), summon)
import Summoner.GhcVer (parseGhcVer, showGhcVer)
import Summoner.License (License (..), LicenseName, fetchLicense, parseLicenseName,
                         showLicenseWithDesc)
import Summoner.Text (intercalateMap)
import Summoner.Tui.Forms (activeCheckboxField, disabledAttr, strField)
import Summoner.Tui.GroupBorder (groupBorder, (|>))
import Summoner.Tui.Kit
import Summoner.Tui.Widget (borderLabel, hArrange, label, listInBorder)

import qualified Brick (on)
import qualified Brick.Main as M
import qualified Brick.Util as U
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as W
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Graphics.Vty as V


-- | Main function that parses @CLI@ arguments and runs @summoner@ in TUI mode.
summonTui :: IO ()
summonTui = summon runTuiCommand

-- | Run TUI specific to each command.
runTuiCommand :: Command -> IO ()
runTuiCommand = \case
    New opts      -> summonTuiNew opts
    ShowInfo opts -> summonTuiShow opts

{- | TUI for creating new project. Contains interactive elements like text input
fields or checkboxes to configure settings for new project.
-}
summonTuiNew :: NewOpts -> IO ()
summonTuiNew _ = do  -- TODO: use 'NewOpts'
    tkForm <- runTuiNew
    putTextLn $ "The starting form state was:" <> show initialSummonKit
    putTextLn $ "The final form state was:" <> show (formState tkForm)

-- | Simply shows info about GHC versions or licenses in TUI.
summonTuiShow :: ShowOpts -> IO ()
summonTuiShow = \case
    GhcList                 -> runTuiShowGhcVersions
    LicenseList Nothing     -> runTuiShowAllLicenses
    LicenseList (Just name) -> runTuiShowLicense name

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
    | GitHubPrivate
    | GitHubTravis
    | GitHubAppVeyor
    deriving (Eq, Ord, Show)

-- Creates the input form from the given initial 'SummonKit'.
mkForm :: forall e . SummonKit -> Form SummonKit e SummonForm
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
        , 2 |> label "GHC versions " @@= editField (projectMeta . ghcs) Ghcs (Just 1) (intercalateMap " " showGhcVer) (traverse parseGhcVer . words . T.intercalate " ") (txt . T.intercalate "\n") id
        ]
   ++ groupBorder "GitHub"
        [ 2 |> setFieldConcat hArrange . radioField (gitHub . enabled)
            [ (True, GitHubEnable, "Enable")
            , (False, GitHubDisable, "Disable")
            ]
        , 1 |> activeCheckboxField (gitHub . private)  isGitHubEnabled GitHubPrivate  "Private"
        , 1 |> activeCheckboxField (gitHub . travis)   isGitHubEnabled GitHubTravis   "Travis"
        , 2 |> activeCheckboxField (gitHub . appVeyor) isGitHubEnabled GitHubAppVeyor "AppVeyor"
        ]
    ) sk
  where
    isGitHubEnabled :: Bool
    isGitHubEnabled = sk ^. gitHub . enabled

    widgetList :: Bool -> LicenseName -> Widget SummonForm
    widgetList p l = C.hCenter $ str $ if p then "[" ++ show l ++ "]" else show l

    arrangeColumns :: [Widget SummonForm] -> Widget SummonForm
    arrangeColumns widgets =
        let (column1, columns2) = splitAt 7 widgets in
        let (tools, column2) = splitAt 2 columns2 in
        hBox [ vBox $ column1 ++ [borderLabel "Tools" $ padRight Max (hArrange tools)]
             , vBox column2
             ]

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (E.editAttr,           V.white `Brick.on` V.cyan)
    , (E.editFocusedAttr,    V.black `Brick.on` V.yellow)
    , (invalidFormInputAttr, V.white `Brick.on` V.red)
    , (focusedFormInputAttr, V.black `Brick.on` V.yellow)
    , (L.listAttr,           V.white `Brick.on` V.blue)
    , (L.listSelectedAttr,   V.blue  `Brick.on` V.white)
    , (disabledAttr,         U.fg V.brightBlack)
    , ("blue-fg",            U.fg V.blue)
    ]

draw :: Form SummonKit e SummonForm -> [Widget SummonForm]
draw f =
    [ vBox
        [ form <+> tree
        , validationErrors (invalidFields f) <+> help
        ]
    ]
  where
    form :: Widget SummonForm
    form = borderLabel "Summon new project" $ padTop (Pad 1) (renderForm f)

    tree :: Widget SummonForm
    tree = W.hLimitPercent 25 $ W.vLimit 23 $ borderLabel "Project Structure" $ vBox
        [ txt $ renderWidgetTree $ formState f
        -- to fill all the space that widget should take.
        , W.fill ' '
        ]

    validationErrors :: [SummonForm] -> Widget SummonForm
    validationErrors formFields = W.hLimitPercent 45 $ borderLabel "Errors" (validationBlock <+> W.fill ' ')
      where
        validationBlock :: Widget SummonForm
        validationBlock = vBox $ case formFields of
            []     -> [str "Project configuration is valid"]
            fields -> map str $ mapMaybe fieldNameErrorMsg fields

        fieldNameErrorMsg :: SummonForm -> Maybe String
        fieldNameErrorMsg = \case
            ProjectName         -> Just "Directory with such name already exists"
            CabalField          -> Just "At least one build tool should be selected"
            Lib                 -> Just "At least library or executable should be selected"
            CustomPreludeModule -> Just "Prelude module cannot be empty if package specified"
            Ghcs                -> Just "Some GHC versions failed to parse"
            _ -> Nothing

    help, helpBody :: Widget SummonForm
    help     = borderLabel "Help" (helpBody <+> W.fill ' ')
    helpBody = vBox
        [ str       "• Esc    : quit"
        , yellowStr "• Yellow" <+> str " : focused input field"
        , redStr    "• Red   " <+> str " : invalid input field"
        , str       "• Ctrk+U : remove input to the start"
        , str       "• Ctrk+K : remove input to the end"
        , str       "• Arrows : up/down arrows to choose license"
        ]

    redStr, yellowStr :: String -> Widget SummonForm
    redStr = withAttr invalidFormInputAttr . str
    yellowStr = withAttr E.editFocusedAttr . str

app :: [FilePath] -> App (Form SummonKit e SummonForm) e SummonForm
app dirs = App
    { appDraw = draw
    , appHandleEvent = \s ev -> case ev of
        VtyEvent V.EvResize {}       -> continue s
        -- TODO: should we cancel on Esc and apply on Ctrl+Enter?
        VtyEvent (V.EvKey V.KEnter [V.MCtrl]) -> halt s
        VtyEvent (V.EvKey V.KEsc []) -> halt s
        VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) -> do
            s' <- handleFormEvent ev s
            continue $
                if focusGetCurrent (formFocus s') == Just UserOwner
                then mkForm $ formState s' & user . owner .~ ""
                else s'

        MouseDown n _ _ _ -> case n of
            GitHubEnable  -> handleFormEvent ev s >>= continue . mkForm . formState
            GitHubDisable -> handleFormEvent ev s >>= continue . mkForm . formState
            _             -> handleFormEvent ev s >>= continue

        _                            -> do
            s' <- handleFormEvent ev s
            let kit = formState s'

            let projectName = kit ^. project . repo
            let doesProjectExist = toString projectName `notElem` dirs


            let cabalOrStack = kit ^. cabal || kit ^. stack
            let libOrExe = kit ^. projectMeta . lib || kit ^. projectMeta . exe

            -- Require age field to contain a value that is at least 18.
            continue $ setFieldValid doesProjectExist ProjectName
                     $ setFieldValid cabalOrStack StackField
                     $ setFieldValid cabalOrStack CabalField
                     $ setFieldValid libOrExe Lib
                     $ setFieldValid libOrExe Exe s'

    , appChooseCursor = focusRingCursor formFocus
    , appStartEvent = pure
    , appAttrMap = const theMap
    }

-- | Runs brick application with given start state.
runApp :: Ord n => App s e n -> s -> IO s
runApp = customMain buildVty Nothing
  where
    buildVty :: IO V.Vty
    buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        pure v

runTuiNew :: IO (Form SummonKit e SummonForm)
runTuiNew = do
    filesAndDirs <- listDirectory =<< getCurrentDirectory
    dirs <- filterM doesDirectoryExist filesAndDirs
    runApp (app dirs) (mkForm initialSummonKit)

-- | Creates simple brick app that doesn't have state and just displays given 'Widget'.
mkSimpleApp :: Widget n -> App () e n
mkSimpleApp w = (simpleApp w)
    { appAttrMap = const theMap
    }

runSimpleApp :: Ord n => Widget n -> IO ()
runSimpleApp w = runApp (mkSimpleApp w) ()

runTuiShowGhcVersions :: IO ()
runTuiShowGhcVersions = runSimpleApp drawGhcVersions
  where
    drawGhcVersions :: Widget ()
    drawGhcVersions = listInBorder "Supported GHC versions" 28 0 (map showGhcVer universe)

runTuiShowAllLicenses :: IO ()
runTuiShowAllLicenses = runSimpleApp drawLicenseNames
  where
    drawLicenseNames :: Widget ()
    drawLicenseNames = listInBorder "Supported licenses" 70 4 (map showLicenseWithDesc universe)

runTuiShowLicense :: String -> IO ()
runTuiShowLicense (toText -> name) = case parseLicenseName name of
    Nothing -> do
        errorMessage $ "Error parsing license name: " <> name
        infoMessage "Use 'summon show license' command to see the list of all available licenses"
    Just licenseName -> do
        lc <- fetchLicense licenseName
        runApp (licenseApp licenseName lc) ()
  where
    licenseApp :: LicenseName -> License -> App () e ()
    licenseApp licenseName lc = App
        { appDraw         = drawScrollableLicense licenseName lc
        , appStartEvent   = pure
        , appAttrMap      = const $ attrMap V.defAttr []
        , appChooseCursor = M.neverShowCursor
        , appHandleEvent  = \() event -> case event of
            VtyEvent (V.EvKey V.KDown []) -> M.vScrollBy licenseScroll   1  >> continue ()
            VtyEvent (V.EvKey V.KUp [])   -> M.vScrollBy licenseScroll (-1) >> continue ()
            VtyEvent (V.EvKey V.KEsc [])  -> halt ()
            _                             -> continue ()
        }


    licenseScroll :: M.ViewportScroll ()
    licenseScroll = M.viewportScroll ()

    drawScrollableLicense :: LicenseName -> License -> () -> [Widget ()]
    drawScrollableLicense licenseName (License lc) = const [ui]
      where
        ui :: Widget ()
        ui = C.center
            $ W.hLimit 80
            $ borderLabel ("License: " ++ show licenseName)
            $ W.viewport () Vertical
            $ vBox
            $ map (\t -> if t == "" then txt "\n" else W.txtWrap t)
            $ lines lc
