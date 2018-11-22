{-# LANGUAGE Rank2Types #-}

{- | This is experimental module.

We are trying to make the TUI app for summoner, but it's WIP.
-}

module Summoner.Tui
       ( summonTui
       ) where

import Brick (App (..), AttrMap, BrickEvent (..), Padding (Pad), Widget, attrMap, continue,
              customMain, hBox, halt, padRight, padTop, str, txt, vBox, vLimit, withAttr, (<+>))
import Brick.Focus (focusGetCurrent, focusRingCursor)
import Brick.Forms (Form, checkboxField, editField, editTextField, focusedFormInputAttr, formFocus,
                    formState, handleFormEvent, invalidFields, invalidFormInputAttr, listField,
                    newForm, radioField, renderForm, setFieldConcat, setFieldValid, setFormConcat,
                    (@@=))
import Lens.Micro ((.~), (^.))
import System.Directory (doesDirectoryExist, getCurrentDirectory, getHomeDirectory, listDirectory)
import System.FilePath ((</>))

import Summoner.CLI (Command (..), runShow, summon)
import Summoner.Config (configT)
import Summoner.Default (defaultTomlFile)
import Summoner.GhcVer (parseGhcVer, showGhcVer)
import Summoner.License (LicenseName)
import Summoner.Text (intercalateMap)
import Summoner.Tui.Forms (activeCheckboxField, disabledAttr, strField)
import Summoner.Tui.GroupBorder (groupBorder, (|>))
import Summoner.Tui.Kit

import qualified Brick (on)
import qualified Brick.Util as U
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Core as W
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Data.Text as T
import qualified Graphics.Vty as V
import qualified Toml


summonTui :: IO ()
summonTui = summon runTuiCommand

runTuiCommand :: Command -> IO ()
runTuiCommand = \case
    -- TODO: parse config and initialize TUI with it
    New _ -> do
        let initialSummonKit = mkSummonKit NewMode
        finalSummonKitForm <- executeSummonTui initialSummonKit
        putTextLn $ "The starting form state was:" <> show initialSummonKit
        putTextLn $ "The final form state was:" <> show (formState finalSummonKitForm)

    Init -> do
        let initialSummonKit = mkSummonKit InitMode
        finalSummonKitForm <- executeSummonTui initialSummonKit
        let tomlConfig = Toml.encode configT $ summonKitToConfig $ formState finalSummonKitForm
        homeDir <- getHomeDirectory
        writeFileText (homeDir </> defaultTomlFile) tomlConfig

    ShowInfo opts -> runShow opts

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
        [ 2 |> label "Owner" @@= editTextField (user . owner) UserOwner (Just 1)
        , 1 |> label "Full name" @@= editTextField (user . fullName) UserFullName (Just 1)
        , 2 |> label "Email" @@= editTextField (user . email) UserEmail (Just 1)
        ]
   ++ groupBorder "Project" (
        [2 |> label "Name"        @@= editTextField (project . repo)     ProjectName (Just 1) | currentMode == NewMode ] ++
        [3 |> label "Description" @@= editTextField (project . desc)     ProjectDesc (Just 2) | currentMode == NewMode ] ++
        [2 |> label "Category"    @@= editTextField (project . category) ProjectCat (Just 1)  | currentMode == NewMode ] ++
        [4 |> vLimit 3 . label "License" @@= listField (const (fromList $ universe @LicenseName))
              maybeLicense widgetList 1 ProjectLicense
        ]
      )
   ++ groupBorder "Tools"
        [ 2 |> checkboxField cabal CabalField "Cabal"
        , 2 |> checkboxField stack StackField "Stack"
        ]
   ++ groupBorder "Project Meta"
        [ 2 |> checkboxField (projectMeta . lib) Lib "Library"
        , 1 |> checkboxField (projectMeta . exe) Exe "Executable"
        , 1 |> checkboxField (projectMeta . test) Test "Tests"
        , 2 |> checkboxField (projectMeta . bench) Bench "Benchmarks"
        , 1 |> strField "Custom prelude"
        , 1 |> label "Name" @@= editTextField (projectMeta . preludeName) CustomPreludeName (Just 1)
        , 2 |> label "Module" @@= editTextField (projectMeta . preludeModule) CustomPreludeModule (Just 1)
        , 2 |> label "GHC versions" @@= editField (projectMeta . ghcs) Ghcs (Just 1) (intercalateMap " " showGhcVer) (traverse parseGhcVer . words . T.intercalate " ") (txt . T.intercalate "\n") id
        ]
   ++ groupBorder "GitHub"
        [ 2 |> setFieldConcat arrangeRadioHoriz . radioField (gitHub . enabled)
            [ (True, GitHubEnable, "Enable")
            , (False, GitHubDisable, "Disable")
            ]
        , 1 |> activeCheckboxField (gitHub . private)  isGitHubEnabled GitHubPrivate  "Private"
        , 1 |> activeCheckboxField (gitHub . travis)   isGitHubEnabled GitHubTravis   "Travis"
        , 2 |> activeCheckboxField (gitHub . appVeyor) isGitHubEnabled GitHubAppVeyor "AppVeyor"
        ]
    ) sk
  where
    currentMode :: KitMode
    currentMode = sk ^. mode

    isGitHubEnabled :: Bool
    isGitHubEnabled = sk ^. gitHub . enabled

    label :: String -> Widget n -> Widget n
    label l w = str l <+>  w

    widgetList :: Bool -> LicenseName -> Widget SummonForm
    widgetList p l = C.hCenter $ str $ if p then "[" ++ show l ++ "]" else show l

    arrangeColumns :: [Widget SummonForm] -> Widget SummonForm
    arrangeColumns widgets =
        let (column1, column2) = splitAt (leftColumnSize currentMode) widgets in
        hBox [ vBox column1
             , vBox column2
             ]

    arrangeRadioHoriz :: [Widget SummonForm] -> Widget SummonForm
    arrangeRadioHoriz = hBox . updateHead (padRight (Pad 2))

    updateHead :: (a -> a) -> [a] -> [a]
    updateHead _ []       = []
    updateHead f (a : as) = f a : as


theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (E.editAttr,           V.white `Brick.on` V.cyan)
    , (E.editFocusedAttr,    V.black `Brick.on` V.yellow)
    , (invalidFormInputAttr, V.white `Brick.on` V.red)
    , (focusedFormInputAttr, V.black `Brick.on` V.yellow)
    , (L.listAttr,           V.white `Brick.on` V.blue)
    , (L.listSelectedAttr,   V.blue  `Brick.on` V.white)
    , (disabledAttr,         U.fg V.brightBlack)
    ]

draw :: Form SummonKit e SummonForm -> [Widget SummonForm]
draw f =
    [ C.vCenter $ vBox
        [ C.hCenter form
      <+> W.hLimitPercent 25 (C.hCenter tree)
        , validationErrors (invalidFields f)
        , help
        ]
    ]
  where
    kit :: SummonKit
    kit = formState f

    form :: Widget SummonForm
    form = B.borderWithLabel (str $ formName $ kit ^. mode) $ padTop (Pad 1) (renderForm f)

    validationErrors :: [SummonForm] -> Widget SummonForm
    validationErrors formFields = B.borderWithLabel (str "Errors") $ case formFields of
        []     -> str "Project configuration is valid"
        fields -> vBox $ map str $ mapMaybe fieldNameErrorMsg fields
      where
        fieldNameErrorMsg :: SummonForm -> Maybe String
        fieldNameErrorMsg = \case
            ProjectName         -> Just "Directory with such name already exists"
            CabalField          -> Just "At least one build tool should be selected"
            Lib                 -> Just "At least library or executable should be selected"
            CustomPreludeModule -> Just "Prelude module cannot be empty if package specified"
            Ghcs                -> Just "Some GHC versions failed to parse"
            _ -> Nothing

    help, helpBody :: Widget SummonForm
    help     = padTop (Pad 1) $ B.borderWithLabel (str "Help") helpBody
    helpBody = vBox
        [       str "• Esc    : quit"
        , yellowStr "• Yellow" <+> str " : focused input field"
        ,    redStr "• Red   " <+> str " : invalid input field"
        ,       str "• Ctrk+U : remove input field content from cursor position to the start"
        ,       str "• Ctrk+K : remove input field content from cursor position to the end"
        ,       str "• Arrows : up/down arrows to choose license"
        ]

    tree :: Widget SummonForm
    tree = B.borderWithLabel (str "Project Structure") $ txt $ renderWidgetTree $ formState f

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

executeSummonTui :: SummonKit -> IO (Form SummonKit e SummonForm)
executeSummonTui kit = do
    filesAndDirs <- listDirectory =<< getCurrentDirectory
    dirs <- filterM doesDirectoryExist filesAndDirs
    customMain buildVty Nothing (app dirs) $ mkForm kit
  where
    buildVty :: IO V.Vty
    buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        pure v
