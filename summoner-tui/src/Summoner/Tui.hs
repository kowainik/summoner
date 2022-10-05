{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module                  : Summoner.Tui
Copyright               : (c) 2018-2022 Kowainik
SPDX-License-Identifier : MPL-2.0
Maintainer              : Kowainik <xrom.xkov@gmail.com>
Stability               : Stable
Portability             : Portable

TUI for the Summoner tool. See demo in the README.md file.
-}

module Summoner.Tui
       ( summonTui
       ) where

import Brick (App (..), AttrMap, BrickEvent (..), Widget, attrMap, attrName, customMain, halt,
              simpleApp, str, txt, vBox, withAttr, (<+>))
import Brick.Focus (focusRingCursor)
import Brick.Forms (allFieldsValid, focusedFormInputAttr, formFocus, formState, handleFormEvent,
                    invalidFormInputAttr, renderForm)
import Brick.Main (ViewportScroll, neverShowCursor, vScrollBy, viewportScroll)
import Brick.Types (EventM, ViewportType (Vertical))
import Brick.Util (fg)
import Brick.Widgets.Border (borderAttr)
import Brick.Widgets.Center (center)
import Brick.Widgets.Core (emptyWidget, fill, hLimit, hLimitPercent, padTopBottom, strWrap, txtWrap,
                           vLimit, viewport)
import Brick.Widgets.Edit (editAttr, editFocusedAttr)
import Brick.Widgets.List (listSelectedAttr, listSelectedFocusedAttr)
import Colourista (errorMessage, infoMessage)
import Lens.Micro ((.~), (^.))
import System.Directory (doesDirectoryExist, doesFileExist, getCurrentDirectory, listDirectory)

import Summoner.CLI (Command (..), NewOpts (..), ShowOpts (..), getCustomLicenseText,
                     getFinalConfig, runConfig, runScript, summon)
import Summoner.Config (ConfigP (cFiles))
import Summoner.Decision (Decision (..))
import Summoner.Default (defaultConfigFile)
import Summoner.GhcVer (ghcTable)
import Summoner.License (License (..), LicenseName, parseLicenseName, showLicenseWithDesc)
import Summoner.Mode (isNonInteractive)
import Summoner.Project (generateProjectNonInteractive, initializeProject)
import Summoner.Source (fetchSources)
import Summoner.Tui.Field (disabledAttr)
import Summoner.Tui.Form (KitForm, SummonForm (..), getCurrentFocus, isActive, mkForm, recreateForm)
import Summoner.Tui.Kit
import Summoner.Tui.Validation (ctrlD, formErrorMessages, handleAutofill, projectDescNewLine,
                                summonFormValidation)
import Summoner.Tui.Widget (borderLabel, listInBorder)

import qualified Brick (on)
import qualified Graphics.Vty as V
import qualified Paths_summoner_tui as Meta (version)


-- | Main function that parses @CLI@ arguments and runs @summoner@ in TUI mode.
summonTui :: IO ()
summonTui = summon Meta.version runTuiCommand

-- | Run TUI specific to each command.
runTuiCommand :: Command -> IO ()
runTuiCommand = \case
    New opts      -> summonTuiNew opts
    ShowInfo opts -> summonTuiShow opts
    Script opts   -> runScript opts  -- TODO: implement TUI for script command
    Config opts   -> runConfig opts

----------------------------------------------------------------------------
-- New command
----------------------------------------------------------------------------

{- | TUI for creating new project. Contains interactive elements like text input
fields or checkboxes to configure settings for new project.
-}
summonTuiNew :: NewOpts -> IO ()
summonTuiNew newOpts@NewOpts{..} = do
    -- configure initial state for TUI application
    finalConfig <- getFinalConfig newOpts
    when (isNonInteractive newOptsInteractivity) $ do
        generateProjectNonInteractive
            newOptsConnectMode
            newOptsProjectName
            finalConfig
        () <$ exitSuccess
    files <- fetchSources newOptsConnectMode (cFiles finalConfig)
    configFilePath <- findConfigFile
    let initialKit = configToSummonKit
            newOptsProjectName
            newOptsConnectMode
            configFilePath
            files
            finalConfig

    -- run TUI app
    skForm <- runTuiNew initialKit

    -- perform actions depending on the final state
    let kit = formState skForm
    if allFieldsValid skForm && (kit ^. shouldSummon == Yes)
    then finalSettings kit >>= initializeProject
    else errorMessage "Aborting summoner"
  where
    findConfigFile :: IO (Maybe FilePath)
    findConfigFile = if newOptsIgnoreFile
        then pure Nothing
        else case newOptsConfigFile of
            Nothing  -> defaultConfigFile >>= \file ->
                ifM (doesFileExist file) (pure $ Just file) (pure Nothing)
            justFile -> pure justFile

-- | Check content of current directory and create form after forming 'SummonKit'.
runTuiNew :: SummonKit -> IO (KitForm e)
runTuiNew kit = do
    filesAndDirs <- listDirectory =<< getCurrentDirectory
    dirs <- filterM doesDirectoryExist filesAndDirs
    runApp (appNew dirs) (summonFormValidation dirs $ mkForm kit)

-- | Represents @new@ command app behaviour.
appNew :: [FilePath] -> App (KitForm e) e SummonForm
appNew dirs = App
    { appDraw = drawNew dirs
    , appHandleEvent = \ev -> do
        s <- get
        if formState s ^. shouldSummon == Idk
        then case ev of
            VtyEvent (V.EvKey V.KEnter []) -> put (changeShouldSummon Yes s) >> halt
            VtyEvent (V.EvKey V.KEsc [])   -> withForm ev (changeShouldSummon Nop)
            _otherKey                      -> pass
        else case ev of
            VtyEvent V.EvResize {} -> pass
            VtyEvent (V.EvKey V.KEnter [V.MMeta]) ->
                withForm ev (validateForm . projectDescNewLine)
            VtyEvent (V.EvKey V.KEnter []) ->
                if allFieldsValid s
                then withForm ev (changeShouldSummon Idk)
                else pass
            VtyEvent (V.EvKey V.KEsc []) -> halt
            VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) ->
                withForm ev (validateForm . ctrlD)

            -- Handle active/inactive checkboxes
            VtyEvent (V.EvKey (V.KChar ' ') []) -> case getCurrentFocus s of
                Nothing    -> withFormDef ev
                Just field -> handleCheckboxActivation ev field

            -- Run handler for autofill actions
            VtyEvent (V.EvKey key [])
                | keyTriggersAutofill key
                -> withForm ev (validateForm . handleAutofill)

            MouseDown n _ _ _ -> handleCheckboxActivation ev n

            -- Handle skip of deactivated checkboxes
            VtyEvent (V.EvKey (V.KChar '\t') []) -> loopWhileInactive ev
            VtyEvent (V.EvKey V.KBackTab     []) -> loopWhileInactive ev

            -- Default action
            _otherEvent -> withFormDef ev

    , appChooseCursor = focusRingCursor formFocus
    , appStartEvent = pass
    , appAttrMap = const theMap
    }
  where
    withForm ev f = handleFormEvent ev >> modify f
    withFormDef ev = withForm ev validateForm

    changeShouldSummon :: Decision -> KitForm e -> KitForm e
    changeShouldSummon newShould f = mkForm $ formState f & shouldSummon .~ newShould

    validateForm :: KitForm e -> KitForm e
    validateForm = summonFormValidation dirs

    mkNewForm :: KitForm e -> KitForm e
    mkNewForm = recreateForm validateForm

    -- Activate/Deactivate checkboxes depending on current field change
    handleCheckboxActivation
        :: BrickEvent SummonForm e
        -> SummonForm
        -> EventM SummonForm (KitForm e) ()
    handleCheckboxActivation ev = \case
        CabalField     -> withForm ev mkNewForm
        StackField     -> withForm ev mkNewForm
        GitHubEnable   -> withForm ev mkNewForm
        GitHubDisable  -> withForm ev mkNewForm
        GitHubNoUpload -> withForm ev mkNewForm
        _nonCheckBox   -> withFormDef ev

    -- Handles form event until current element is active
    loopWhileInactive
        :: BrickEvent SummonForm e
        -> EventM SummonForm (KitForm e) ()
    loopWhileInactive ev = do
        handleFormEvent ev
        newForm <- get
        case getCurrentFocus newForm of
            Nothing -> pass
            Just field -> if not $ isActive (formState newForm) field
                then loopWhileInactive ev
                else pass

    -- Autofill is only triggered on characters and backspace keys.
    keyTriggersAutofill :: V.Key -> Bool
    keyTriggersAutofill (V.KChar _) = True
    keyTriggersAutofill V.KBS       = True
    keyTriggersAutofill _otherKey   = False

-- | Draws the form for @new@ command.
drawNew :: [FilePath] -> KitForm e -> [Widget SummonForm]
drawNew dirs kitForm = case kit ^. shouldSummon of
    Idk -> [confirmDialog]
    Yes -> [formWidget]
    Nop -> [formWidget]
  where
    kit :: SummonKit
    kit = formState kitForm

    confirmDialog :: Widget SummonForm
    confirmDialog = center $ hLimit 55 $ borderLabel "Confirm" $ padTopBottom 2 $ vBox
        [ str "• Enter – Press Enter to create the project"
        , str "• Esc   – Or Esc to go back to settings"
        ]

    formWidget :: Widget SummonForm
    formWidget = vBox
        [ form   <+> tree
        , status <+> help
        ]

    form :: Widget SummonForm
    form = borderLabel "Summon new project" (renderForm kitForm)

    tree :: Widget SummonForm
    tree = hLimitPercent 25 $ vLimit 22 $ borderLabel "Project Structure" $ vBox
        [ withAttr (attrName "tree") $ txt $ renderWidgetTree kit
        -- to fill all the space that widget should take.
        , fill ' '
        ]

    status :: Widget SummonForm
    status = hLimitPercent 45 $
        borderLabel "Status" $ vBox
            [ informationBlock
            , validationBlock
            , configBlock
            , fill ' '
            ]
      where
        informationBlock :: Widget SummonForm
        informationBlock = case getCurrentFocus kitForm of
            Just UserOwner   -> infoTxt "GitHub username"
            Just ProjectCat  -> infoTxt "Comma-separated categories as used at Hackage"
            Just Ghcs        -> infoTxt "Space separated GHC versions"
            _noInfoAvailable -> emptyWidget

        infoTxt :: Text -> Widget SummonForm
        infoTxt = withAttr (attrName "blue-fg") . txtWrap . (<>) " ⓘ  "

        validationBlock :: Widget SummonForm
        validationBlock = vBox $ case formErrorMessages dirs kitForm of
            []     -> [withAttr (attrName "green-fg") $ str " ✔  Project configuration is valid"]
            fields -> map (\msg -> withAttr (attrName "red-fg") $ strWrap $ " ☓  " ++ msg) fields

        configBlock :: Widget SummonForm
        configBlock = case kit ^. configFile of
            Nothing   -> emptyWidget
            Just file -> infoTxt $ toText file <> " file is used"

    help, helpBody :: Widget SummonForm
    help     = borderLabel "Help" (helpBody <+> fill ' ')
    helpBody = vBox
        [ str "• Enter     : create the project"
        , str "• Esc       : quit"
        , str "• Ctrl+d    : remove input of the text field"
        , str "• Arrows    : up/down arrows to choose license"
        , str "• Alt+Enter : switch to new line"
        ]

----------------------------------------------------------------------------
-- Show command
----------------------------------------------------------------------------

-- | Simply shows info about GHC versions or licenses in TUI.
summonTuiShow :: ShowOpts -> IO ()
summonTuiShow = \case
    GhcList                 -> runTuiShowGhcVersions
    LicenseList Nothing     -> runTuiShowAllLicenses
    LicenseList (Just name) -> runTuiShowLicense name

runTuiShowGhcVersions :: IO ()
runTuiShowGhcVersions = runSimpleApp drawGhcVersions
  where
    drawGhcVersions :: Widget ()
    drawGhcVersions = listInBorder "Supported GHC versions" 60 0 ghcTable

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
        lc <- getCustomLicenseText licenseName
        runApp (licenseApp licenseName lc) ()
  where
    licenseApp :: LicenseName -> License -> App () e ()
    licenseApp licenseName lc = App
        { appDraw         = drawScrollableLicense licenseName lc
        , appStartEvent   = pass
        , appAttrMap      = const theMap
        , appChooseCursor = neverShowCursor
        , appHandleEvent  = \case
            VtyEvent (V.EvKey V.KDown []) -> vScrollBy licenseScroll   1  >> pass
            VtyEvent (V.EvKey V.KUp [])   -> vScrollBy licenseScroll (-1) >> pass
            VtyEvent (V.EvKey V.KEsc [])  -> halt
            _otherEvent                   -> pass
        }

    licenseScroll :: ViewportScroll ()
    licenseScroll = viewportScroll ()

    drawScrollableLicense :: LicenseName -> License -> () -> [Widget ()]
    drawScrollableLicense licenseName (License lc) = const [ui]
      where
        ui :: Widget ()
        ui = center
            $ hLimit 80
            $ borderLabel ("License: " ++ show licenseName)
            $ viewport () Vertical
            $ vBox
            $ map (\t -> if t == "" then txt "\n" else txtWrap t)
            $ lines lc

----------------------------------------------------------------------------
-- Internal
----------------------------------------------------------------------------

-- | Runs brick application with given start state.
runApp :: Ord n => App s e n -> s -> IO s
runApp app s = do
    initialVty <- buildVty
    customMain initialVty buildVty Nothing app s
  where
    buildVty :: IO V.Vty
    buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        pure v

-- | Runs the app without any state.
runSimpleApp :: Ord n => Widget n -> IO ()
runSimpleApp w = runApp (mkSimpleApp w) ()

-- | Creates simple brick app that doesn't have state and just displays given 'Widget'.
mkSimpleApp :: Widget n -> App () e n
mkSimpleApp w = (simpleApp w)
    { appAttrMap = const theMap
    }

-- | Styles, colours that are used across the app.
theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (editAttr,                V.black `Brick.on` V.cyan)
    , (editFocusedAttr,         V.black `Brick.on` V.white)
    , (invalidFormInputAttr,    V.white `Brick.on` V.red)
    , (focusedFormInputAttr,    V.black `Brick.on` V.yellow)
    , (listSelectedAttr,        V.black `Brick.on` V.cyan)
    , (listSelectedFocusedAttr, V.black `Brick.on` V.white)
    , (disabledAttr,            fg V.brightBlack)
    , (attrName "blue-fg",      fg V.blue)
    , (attrName "green-fg",     fg V.green)
    , (attrName "yellow-fg",    fg V.yellow)
    , (attrName "red-fg",       fg V.brightRed)
    , (borderAttr,              fg V.cyan)
    , (attrName "tree",         fg V.cyan)
    ]
