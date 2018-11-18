{-# LANGUAGE Rank2Types #-}

{- | This is experimental module.

We are trying to make the TUI app for summoner, but it's WIP.
-}

module Summoner.Tui
       ( summonTui
       ) where

import Brick (App (..), AttrMap, BrickEvent (VtyEvent), Padding (Pad), Widget, attrMap, continue,
              customMain, hBox, halt, padTop, str, vBox, (<+>), (<=>))
import Brick.Focus (focusRingCursor)
import Brick.Forms (Form, FormFieldState (..), checkboxField, editTextField, focusedFormInputAttr,
                    formFocus, formState, handleFormEvent, invalidFormInputAttr, newForm,
                    renderForm, setFormConcat, (@@=))
import Lens.Micro (Lens')

import Summoner.Tui.CheckBox (CheckBox (..), checkBoxL)
import Summoner.Tui.GroupBorder (groupBorder)
import Summoner.Tui.Kit (SummonKit (..), email, fullName, initialSummonKit, owner, tools, user)

import qualified Brick (on)
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Center as C
import qualified Brick.Widgets.Edit as E
import qualified Graphics.Vty as V


summonTui :: IO ()
summonTui = do
    tkForm <- executeSummonTui
    putTextLn $ "The starting form state was:" <> show initialSummonKit
    putTextLn $ "The final form state was:" <> show (formState tkForm)


data SummonForm
    = UserOwner
    | UserFullName
    | UserEmail
    | ToolsBox Int
    deriving (Eq, Ord, Show)

type SummonField e = FormFieldState SummonKit e SummonForm

-- Creates the inout form from the given initial 'TreasureChest'.
mkForm :: forall e . SummonKit -> Form SummonKit e SummonForm
mkForm sk@SummonKit{..} = setFormConcat myBox $ newForm
    ( groupBorder "User"
        [ label "Owner" @@= editTextField (user . owner) UserOwner (Just 1)
        , label "Full name" @@= editTextField (user . fullName) UserFullName (Just 1)
        , label "Email" @@= editTextField (user . email) UserEmail (Just 1)
        ]
   ++ toCheckBoxGroup  "Tools" tools ToolsBox  summonKitTools
    ) sk
  where
    label :: String -> Widget n -> Widget n
    label s w = str s <+>  w

    toCheckBoxGroup
        :: forall a . Show a
        => String
        -> Lens' SummonKit [CheckBox a]
        -> (Int -> SummonForm)
        -> [CheckBox a]
        -> [SummonKit -> SummonField e]
    toCheckBoxGroup groupName kitL field ch = groupBorder groupName
        ( zipWith makeCheckBox [0..] ch)
      where
        makeCheckBox :: Int -> CheckBox a -> SummonKit -> SummonField e
        makeCheckBox i CheckBox{..} = checkboxField
            (kitL . checkBoxL i)
            (field i)
            (show checkboxData)

    myBox :: [Widget SummonForm] -> Widget SummonForm
    myBox ws = let (a, b) = splitAt 3 ws in
        hBox [vBox a, vBox b]

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (E.editAttr,           V.white `Brick.on` V.black)
    , (E.editFocusedAttr,    V.black `Brick.on` V.yellow)
    , (invalidFormInputAttr, V.white `Brick.on` V.red)
    , (focusedFormInputAttr, V.black `Brick.on` V.yellow)
    ]

draw :: Form SummonKit e SummonForm -> [Widget SummonForm]
draw f = [C.vCenter $ C.hCenter form <=> C.hCenter help]
  where
    form = B.borderWithLabel (str "Form") $ padTop (Pad 1) (renderForm f)
    help = padTop (Pad 1) $ B.borderWithLabel (str "Help") body
    body = str "- Enter/Esc quit, mouse interacts with fields"

app :: App (Form SummonKit e SummonForm) e SummonForm
app = App
    { appDraw = draw
    , appHandleEvent = \s ev -> case ev of
        VtyEvent V.EvResize {}       -> continue s
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
