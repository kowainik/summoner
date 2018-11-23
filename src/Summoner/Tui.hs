{-# LANGUAGE Rank2Types   #-}
{-# LANGUAGE ViewPatterns #-}

{- | This is experimental module.

We are trying to make the TUI app for summoner, but it's WIP.
-}

module Summoner.Tui
       ( summonTui
       ) where

import Brick (App (..), AttrMap, BrickEvent (..), Padding (Pad), Widget, attrMap, continue,
              customMain, halt, padTop, simpleApp, str, txt, vBox, withAttr, (<+>))
import Brick.Focus (focusRingCursor)
import Brick.Forms (Form, focusedFormInputAttr, formFocus, formState, handleFormEvent,
                    invalidFields, invalidFormInputAttr, renderForm)
import Brick.Types (ViewportType (Vertical))
import System.Directory (doesDirectoryExist, getCurrentDirectory, listDirectory)

import Summoner.Ansi (errorMessage, infoMessage)
import Summoner.CLI (Command (..), NewOpts, ShowOpts (..), summon)
import Summoner.GhcVer (showGhcVer)
import Summoner.License (License (..), LicenseName, fetchLicense, parseLicenseName,
                         showLicenseWithDesc)
import Summoner.Tui.Field (disabledAttr)
import Summoner.Tui.Form (SummonForm (..), mkForm)
import Summoner.Tui.Kit
import Summoner.Tui.Validation (ctrlD, fieldNameErrorMsg, summonFormValidation)
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
    tree = W.hLimitPercent 25 $ W.vLimit 22 $ borderLabel "Project Structure" $ vBox
        [ txt $ renderWidgetTree $ formState f
        -- to fill all the space that widget should take.
        , W.fill ' '
        ]

    validationErrors :: [SummonForm] -> Widget SummonForm
    validationErrors formFields = W.hLimitPercent 45 $
        borderLabel "Errors" (validationBlock <+> W.fill ' ')
      where
        validationBlock :: Widget SummonForm
        validationBlock = vBox $ case formFields of
            []     -> [str "Project configuration is valid"]
            fields -> map str $ mapMaybe fieldNameErrorMsg fields

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
        VtyEvent V.EvResize {} -> continue s
        -- TODO: successful exit
        VtyEvent (V.EvKey V.KEnter []) -> halt s
        VtyEvent (V.EvKey V.KEsc []) -> halt s
        VtyEvent (V.EvKey (V.KChar 'd') [V.MCtrl]) ->
            withForm ev s (summonFormValidation dirs . ctrlD)
        MouseDown n _ _ _ -> case n of
            GitHubEnable  -> withForm ev s (mkForm . formState)
            GitHubDisable -> withForm ev s (mkForm . formState)
            _             -> withForm ev s id
        _ -> withForm ev s (summonFormValidation dirs)

    , appChooseCursor = focusRingCursor formFocus
    , appStartEvent = pure
    , appAttrMap = const theMap
    }
  where
    withForm ev s f = handleFormEvent ev s >>= continue . f

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
