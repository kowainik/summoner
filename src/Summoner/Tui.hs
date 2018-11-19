{-# LANGUAGE Rank2Types #-}

{- | This is experimental module.

We are trying to make the TUI app for summoner, but it's WIP.
-}

module Summoner.Tui
       ( summonTui
       ) where

import Brick (App (..), AttrMap, BrickEvent (VtyEvent), Padding (Pad), Widget, attrMap, continue,
              customMain, hBox, halt, padRight, padTop, str, vBox, vLimit, withAttr, (<+>), (<=>))
import Brick.Focus (focusRingCursor)
import Brick.Forms (Form, checkboxField, editTextField, focusedFormInputAttr, formFocus, formState,
                    handleFormEvent, invalidFormInputAttr, listField, newForm, radioField,
                    renderForm, setFieldConcat, setFormConcat, (@@=))

import Summoner.License (LicenseName)
import Summoner.Tui.GroupBorder (groupBorder, (|>))
import Summoner.Tui.Kit (SummonKit (..), appVeyor, cabal, category, desc, email, enabled, fullName,
                         gitHub, initialSummonKit, maybeLicense, owner, private, project, repo,
                         stack, travis, user)

import qualified Brick (on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Brick.Widgets.List as L
import qualified Graphics.Vty as V


summonTui :: IO ()
summonTui = do
    tkForm <- executeSummonTui
    putTextLn $ "The starting form state was:" <> show initialSummonKit
    putTextLn $ "The final form state was:" <> show (formState tkForm)


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

     -- Build tools
    | ToolsBox Int

      -- github fields
    | GitHubEnable
    | GitHubDisable
    | GitHubPrivate
    | GitHubTravis
    | GitHubAppVeyor
    deriving (Eq, Ord, Show)

-- Creates the inout form from the given initial 'TreasureChest'.
mkForm :: forall e . SummonKit -> Form SummonKit e SummonForm
mkForm = setFormConcat arrangeColumns . newForm
    ( groupBorder "User"
        [ 2 |> label "Owner" @@= editTextField (user . owner) UserOwner (Just 1)
        , 1 |> label "Full name" @@= editTextField (user . fullName) UserFullName (Just 1)
        , 2 |> label "Email" @@= editTextField (user . email) UserEmail (Just 1)
        ]
   ++ groupBorder "Project"
        [ 2 |> label "Name" @@= editTextField (project . repo) ProjectName (Just 1)
        , 2 |> label "Description" @@= editTextField (project . desc) ProjectDesc (Just 2)
        , 1 |> label "Category" @@= editTextField (project . category) ProjectCat (Just 1)
        , 4 |> vLimit 3 . label "License" @@= listField (const (fromList $ universe @LicenseName))
              maybeLicense widgetList 1 ProjectLicense
        ]
   ++ groupBorder "Tools"
        [ 2 |> checkboxField cabal CabalField "Cabal"
        , 2 |> checkboxField stack StackField "Stack"
        ]
   ++ groupBorder "GitHub"
        [ 2 |> setFieldConcat arrangeRadioHoriz . radioField (gitHub . enabled)
            [ (True, GitHubEnable, "Enable")
            , (False, GitHubDisable, "Disable")
            ]
        , 1 |> checkboxField (gitHub . private)  GitHubPrivate  "Private"
        , 1 |> checkboxField (gitHub . travis)   GitHubTravis   "Travis"
        , 2 |> checkboxField (gitHub . appVeyor) GitHubAppVeyor "AppVeyor"
        ]
    )
  where
    label :: String -> Widget n -> Widget n
    label s w = str s <+>  w

    widgetList :: Bool -> LicenseName -> Widget SummonForm
    widgetList p l = C.hCenter $ if p then str ("[" ++ show l ++ "]") else str $ show l

    arrangeColumns :: [Widget SummonForm] -> Widget SummonForm
    arrangeColumns widgets =
        let (column1, columns23) = splitAt 7 widgets in
        let (column2, column3)   = splitAt 2 columns23 in
        hBox [ vBox column1
             , vBox column2
             , vBox column3
             ]

    arrangeRadioHoriz :: [Widget SummonForm] -> Widget SummonForm
    arrangeRadioHoriz = hBox . updateHead (padRight (Pad 2))

    updateHead :: (a -> a) -> [a] -> [a]
    updateHead _ []       = []
    updateHead f (a : as) = f a : as


theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (E.editAttr,           V.white `Brick.on` V.black)
    , (E.editFocusedAttr,    V.black `Brick.on` V.yellow)
    , (invalidFormInputAttr, V.white `Brick.on` V.red)
    , (focusedFormInputAttr, V.black `Brick.on` V.yellow)
    , (L.listAttr,           V.white `Brick.on` V.blue)
    , (L.listSelectedAttr,   V.blue  `Brick.on` V.white)
    ]

draw :: Form SummonKit e SummonForm -> [Widget SummonForm]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
  where
    form :: Widget SummonForm
    form = B.borderWithLabel (str "Summon new project") $ padTop (Pad 1) (renderForm f)

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

    redStr, yellowStr :: String -> Widget SummonForm
    redStr = withAttr invalidFormInputAttr . str
    yellowStr = withAttr E.editFocusedAttr . str

app :: App (Form SummonKit e SummonForm) e SummonForm
app = App
    { appDraw = draw
    , appHandleEvent = \s ev -> case ev of
        VtyEvent V.EvResize {}       -> continue s
        -- TODO: should we cancel on Esc and apply on Ctrl+Enter?
        VtyEvent (V.EvKey V.KEsc []) -> halt s
        _                            -> handleFormEvent ev s >>= continue
    , appChooseCursor = focusRingCursor formFocus
    , appStartEvent = pure
    , appAttrMap = const theMap
    }

executeSummonTui :: IO (Form SummonKit e SummonForm)
executeSummonTui = customMain buildVty Nothing app $ mkForm initialSummonKit
  where
    buildVty :: IO V.Vty
    buildVty = do
        v <- V.mkVty =<< V.standardIOConfig
        V.setMode (V.outputIface v) V.Mouse True
        pure v
