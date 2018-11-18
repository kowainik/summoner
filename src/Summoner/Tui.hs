{-# LANGUAGE Rank2Types #-}

{- | This is experimental module.

We are trying to make the TUI app for summoner, but it's WIP.
-}

module Summoner.Tui
       ( summonTui
       ) where

import Brick (App (..), AttrMap, BrickEvent (VtyEvent), Padding (Pad), Widget, attrMap, continue,
              customMain, hBox, halt, padTop, str, vBox, (<=>))
import Brick.Focus (focusRingCursor)
import Brick.Forms (Form, FormFieldState (..), checkboxField, focusedFormInputAttr, formFocus,
                    formState, handleFormEvent, invalidFormInputAttr, newForm, renderForm,
                    setFormConcat)
import Lens.Micro (Lens')

import Summoner.Tui.CheckBox (CheckBox (..), checkBoxL)
import Summoner.Tui.GroupBorder (groupBorder)
import Summoner.Tui.Kit (SummonKit (..), initialSummonKit, test1L, test2L)

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
    = Test1Field Int
    | Test2Field Int
    | UserField Int
    deriving (Eq, Ord, Show)

type SummonField e = FormFieldState SummonKit e SummonForm

-- Creates the inout form from the given initial 'TreasureChest'.
mkForm :: forall e . SummonKit -> Form SummonKit e SummonForm
mkForm sk@SummonKit{..} = setFormConcat myBox $ newForm
    ( toCheckBoxGroup "Test1" test1L Test1Field summonKitTest1L
   ++ toCheckBoxGroup "Test2" test2L Test2Field summonKitTest2L
    ) sk
  where
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

--    toEditFieldGroup
--        :: forall a . Show a
--        => String
--        -> Lens' SummonKit [EditField]
--        -> (Int -> SummonForm)
--        -> [EditField]
--        -> [SummonKit -> SummonField e]
--    toEditFieldGroup groupName kitL field ef = groupBorder groupName
--        (zipWith makeEditField [0..] ch)
--      where
--        makeEditField :: Int -> CheckBox a -> SummonKit -> SummonField e
--        makeEditField i EditField{..} = editTextField
--            (kitL . checkBoxL i)
--            (field i)
--            (show checkboxData)

    myBox :: [Widget SummonForm] -> Widget SummonForm
    myBox ws = let (a, b) = splitAt (length summonKitTest1L) ws in
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
