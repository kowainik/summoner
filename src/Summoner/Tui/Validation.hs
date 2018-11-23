{-# LANGUAGE Rank2Types #-}

-- | This module contains function to validate Form fields.

module Summoner.Tui.Validation
       ( ctrlD
       , summonFormValidation
       , fieldNameErrorMsg
       ) where

import Brick.Focus (focusGetCurrent)
import Brick.Forms (Form, formFocus, formState, setFieldValid)
import Lens.Micro (Lens', (.~), (^.))

import Summoner.Tui.Form (SummonForm (..), mkForm)
import Summoner.Tui.Kit


type KitForm e = Form SummonKit e SummonForm

-- | Clears the 'Text' fields by @Ctrl + d@ key combination.
ctrlD :: KitForm e -> KitForm e
ctrlD =
      clearField "" UserFullName (user . fullName)
    . clearField "" UserEmail (user . email)
    . clearField "" ProjectName (project . repo)
    . clearField "" ProjectDesc (project . desc)
    . clearField "" ProjectCat (project . category)
    . clearField "" CustomPreludeName (projectMeta . preludeName)
    . clearField "" CustomPreludeModule (projectMeta . preludeModule)
    . clearField [] Ghcs (projectMeta . ghcs)
    . clearField "" UserOwner (user . owner)
  where
    clearField :: a -> SummonForm -> Lens' SummonKit a -> KitForm e -> KitForm e
    clearField nil formField fieldLens f =
        if focusGetCurrent (formFocus f) == Just formField
        then mkForm $ formState f & fieldLens .~ nil
        else f

-- | Validates the main @new@ command form.
summonFormValidation :: [FilePath] -> KitForm e -> KitForm e
summonFormValidation dirs f =
      setFieldValid doesProjectExist ProjectName
    . setFieldValid cabalOrStack StackField
    . setFieldValid cabalOrStack CabalField
    . setFieldValid libOrExe Lib
    . setFieldValid libOrExe Exe
    $ f
  where
    kit :: SummonKit
    kit = formState f

    projectName :: Text
    projectName = kit ^. project . repo

    doesProjectExist, cabalOrStack, libOrExe :: Bool
    doesProjectExist = toString projectName `notElem` dirs
    cabalOrStack = kit ^. cabal || kit ^. stack
    libOrExe = kit ^. projectMeta . lib || kit ^. projectMeta . exe

-- | Creates the error messages for 'SummonForm' invalid data.
fieldNameErrorMsg :: SummonForm -> Maybe String
fieldNameErrorMsg = \case
    ProjectName         -> Just "Directory with such name already exists"
    CabalField          -> Just "At least one build tool should be selected"
    Lib                 -> Just "At least library or executable should be selected"
    CustomPreludeModule -> Just "Prelude module cannot be empty if package specified"
    Ghcs                -> Just "Some GHC versions failed to parse"
    _ -> Nothing
