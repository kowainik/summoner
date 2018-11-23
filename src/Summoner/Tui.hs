{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE ViewPatterns #-}

{- | This is experimental module.

We are trying to make the TUI app for summoner, but it's WIP.
-}

module Summoner.Tui
       ( summonTui
       ) where

import Brick (App (..), AttrMap, BrickEvent (..), Padding (Pad), Widget, attrMap, continue,
              customMain, halt, padTop, simpleApp, str, txt, vBox, withAttr, (<+>), (<=>))
import Brick.Focus (focusRingCursor)
import Brick.Forms (allFieldsValid, focusedFormInputAttr, formFocus, formState, handleFormEvent,
                    invalidFormInputAttr, renderForm)
import Brick.Types (ViewportType (Vertical))
import Lens.Micro ((.~), (^.))
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)

import Summoner.Ansi (errorMessage, infoMessage)
import Summoner.CLI (Command (..), NewOpts (..), ShowOpts (..), getFinalConfig, summon)
import Summoner.GhcVer (showGhcVer)
import Summoner.License (License (..), LicenseName, fetchLicense, parseLicenseName,
                         showLicenseWithDesc)
import Summoner.Project (initializeProject)
import Summoner.Tui.Field (disabledAttr)
import Summoner.Tui.Form (KitForm, SummonForm (..), mkForm, recreateForm)
import Summoner.Tui.Kit
import Summoner.Tui.Validation (ctrlD, formErrorMessages, summonFormValidation)
import Summoner.Tui.Widget (borderLabel, listInBorder)

import qualified Brick (on)
import qualified Brick.Main as M
import qualified Brick.Util as U
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as W
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
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
summonTuiNew newOpts@NewOpts{..} = do
    finalConfig <- getFinalConfig newOpts
    let initialKit = configToSummonKit newOptsProjectName newOptsNoUpload newOptsOffline finalConfig
    skForm <- runTuiNew initialKit
    let kit = formState skForm
    if allFieldsValid skForm && kit ^. shouldSummon
    then finalSettings kit >>= initializeProject
    else errorMessage "Aborting summoner"

-- | Simply shows info about GHC versions or licenses in TUI.
summonTuiShow :: ShowOpts -> IO ()
summonTuiShow = \case
    GhcList                 -> runTuiShowGhcVersions
    LicenseList Nothing     -> runTuiShowAllLicenses
    LicenseList (Just name) -> runTuiShowLicense name

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

draw :: [FilePath] -> KitForm e -> [Widget SummonForm]
draw dirs f =
    [ vBox
        [ form <+> tree
        , validationErrors <+> help
        ]
    ]
  where
    form :: Widget SummonForm
    form = borderLabel "Summon new project" $ padTop (Pad 1) (renderForm f)

    tree :: Widget SummonForm
    tree = W.hLimitPercent 25 $ W.vLimit 22 $ borderLabel "Project Structure" $ vBox
        [ txt $ renderWidgetTree $ formState f
        -- to fill all the space that widget should take.
        , W.fill ' '
        ]

    validationErrors :: Widget SummonForm
    validationErrors = W.hLimitPercent 45 $
        borderLabel "Errors" (validationBlock <=> W.fill ' ')
      where
        validationBlock :: Widget SummonForm
        validationBlock = vBox $ case formErrorMessages dirs f of
            []     -> [str "✔ Project configuration is valid"]
            fields -> map (\msg -> W.strWrap $ "❌ " ++ msg) fields

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

app :: [FilePath] -> App (KitForm e) e SummonForm
app dirs = App
    { appDraw = draw dirs
    , appHandleEvent = \s ev -> case ev of
        VtyEvent V.EvResize {} -> continue s
        VtyEvent (V.EvKey V.KEnter []) ->
            if allFieldsValid s
            then halt $ mkForm $ formState s & shouldSummon .~ True
            else continue s
        VtyEvent (V.EvKey V.KEsc []) -> halt s
        VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) ->
            withForm ev s (validateForm . ctrlD)
        MouseDown n _ _ _ -> case n of
            GitHubEnable   -> withForm ev s mkNewForm
            GitHubDisable  -> withForm ev s mkNewForm
            GitHubNoUpload -> withForm ev s mkNewForm
            _              -> withForm ev s id
        _ -> withForm ev s validateForm

    , appChooseCursor = focusRingCursor formFocus
    , appStartEvent = pure
    , appAttrMap = const theMap
    }
  where
    withForm ev s f = handleFormEvent ev s >>= continue . f

    validateForm :: KitForm e -> KitForm e
    validateForm = summonFormValidation dirs

    mkNewForm :: KitForm e -> KitForm e
    mkNewForm = recreateForm validateForm

-- | Runs brick application with given start state.
runApp :: Ord n => App s e n -> s -> IO s
runApp = customMain buildVty Nothing
  where
    buildVty :: IO V.Vty
    buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        pure v

runTuiNew :: SummonKit -> IO (KitForm e)
runTuiNew kit = do
    filesAndDirs <- listDirectory =<< getCurrentDirectory
    dirs <- filterM doesDirectoryExist filesAndDirs
    runApp (app dirs) (summonFormValidation dirs $ mkForm kit)

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
    drawGhcVersions = listInBorder "Supported GHC versions" 30 0 (map showGhcVer universe)

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
        , appAttrMap      = const theMap
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
