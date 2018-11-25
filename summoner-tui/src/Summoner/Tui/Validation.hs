{-# LANGUAGE Rank2Types #-}

-- | This module contains function to validate Form fields.

module Summoner.Tui.Validation
       ( ctrlD
       , summonFormValidation
       , formErrorMessages
       ) where

import Brick.Forms (formState, invalidFields, setFieldValid, setFormFocus)
import Lens.Micro (Lens', (.~), (^.))

import Summoner.Tui.Form (KitForm, SummonForm (..), getCurrentFocus, mkForm)
import Summoner.Tui.Kit

import qualified Data.Text as T


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
        if getCurrentFocus f == Just formField
        then setFormFocus formField $ mkForm $ formState f & fieldLens .~ nil
        else f

-- | Validates the main @new@ command form.
summonFormValidation :: forall e . [FilePath] -> KitForm e -> KitForm e
summonFormValidation dirs kitForm = foldr setValidation kitForm universe
  where
    kit :: SummonKit
    kit = formState kitForm

    wrongFields :: [SummonForm]
    wrongFields = case validateKit dirs kit of
        Success _      -> []
        Failure errors -> concatMap (toList . errorToInvalidFields) errors

    setValidation :: SummonForm -> KitForm e -> KitForm e
    setValidation field = setFieldValid (field `notElem` wrongFields) field

-- | This data type represents all possible errors that can happen during
-- validation of form input fields.
data FormError
    -- | List of empty fields that shouldn't be empty.
    = EmptyFields (NonEmpty SummonForm)
    -- | List of fields that should be exactly one word.
    | OneWord (NonEmpty SummonForm)
    -- | Project with such name already exist.
    | ProjectExist
    -- | At least one build tool should be chosen.
    | CabalOrStack
    -- | At least library or executable should be selected.
    | LibOrExe

-- | Show 'FormError' to display later in TUI.
showFormError :: FormError -> String
showFormError = \case
    ProjectExist -> "Directory with such name already exists"
    CabalOrStack -> "Choose at least one: Cabal or Stack"
    LibOrExe     -> "Choose at least one: Library or Executable"
    EmptyFields fields -> "These fields must not be empty: " ++ joinFields fields
    OneWord fields -> "These fields should contain exactly one word: " ++ joinFields fields
  where
    joinFields :: NonEmpty SummonForm -> String
    joinFields = intercalate ", " . mapMaybe showField . toList

    showField :: SummonForm -> Maybe String
    showField = \case
        UserOwner           -> Just "Owner"
        UserFullName        -> Just "Full name"
        UserEmail           -> Just "Email"
        ProjectName         -> Just "Name"
        ProjectDesc         -> Just "Description"
        ProjectCat          -> Just "Category"
        CustomPreludeName   -> Just "Prelude name"
        CustomPreludeModule -> Just "Module"
        _ -> Nothing

-- | Returns list of all invalid fields according to the error.
errorToInvalidFields :: FormError -> NonEmpty SummonForm
errorToInvalidFields = \case
    EmptyFields fields -> fields
    OneWord fields     -> fields
    ProjectExist       -> one ProjectName
    CabalOrStack       -> CabalField :| [StackField]
    LibOrExe           -> Lib :| [Exe]

-- | Takes boolean value and error and returns error if predicate 'True'.
toError :: Bool -> e -> Validation (NonEmpty e) ()
toError p e = if p then Failure (one e) else Success ()

-- | Validates 'SummonKit' and returns list of all possible errors or success.
validateKit :: [FilePath] -> SummonKit -> Validation (NonEmpty FormError) ()
validateKit dirs kit =
       validateEmpty
    *> validateOneWord
    *> validateProjectExist
    *> validateBuildTools
    *> validateLibOrExe
  where
    liftValidation
        :: (e -> FormError)
        -> Validation e ()
        -> Validation (NonEmpty FormError) ()
    liftValidation mkError = first (one . mkError)

    validateEmpty :: Validation (NonEmpty FormError) ()
    validateEmpty = liftValidation EmptyFields validateFields
      where
        validateFields :: Validation (NonEmpty SummonForm) ()
        validateFields =
               checkField (user . owner) UserOwner
            *> checkField (user . fullName) UserFullName
            *> checkField (user . email) UserEmail
            *> checkField (project . repo) ProjectName
            *> checkField (project . desc) ProjectDesc
            *> toError isEmptyPrelude CustomPreludeModule

        checkField :: Lens' SummonKit Text -> SummonForm -> Validation (NonEmpty SummonForm) ()
        checkField textL = toError $ isEmpty $ kit ^. textL

        isEmpty :: Text -> Bool
        isEmpty t = T.strip t == ""

        isEmptyPrelude :: Bool
        isEmptyPrelude =
               not (isEmpty $ kit ^. projectMeta . preludeName)
            && isEmpty (kit ^. projectMeta . preludeModule)

    validateOneWord :: Validation (NonEmpty FormError) ()
    validateOneWord = liftValidation OneWord validateFields
      where
        validateFields :: Validation (NonEmpty SummonForm) ()
        validateFields =
               checkField (user . owner) UserOwner
            *> checkField (user . email) UserEmail
            *> checkField (project . repo) ProjectName
            *> checkField (projectMeta . preludeName) CustomPreludeName
            *> checkField (projectMeta . preludeModule) CustomPreludeModule

        checkField :: Lens' SummonKit Text -> SummonForm -> Validation (NonEmpty SummonForm) ()
        checkField textL = toError (length (words $ kit ^. textL) > 1)

    validateProjectExist :: Validation (NonEmpty FormError) ()
    validateProjectExist = toError
        (toString (kit ^. project . repo) `elem` dirs)
        ProjectExist

    validateBuildTools :: Validation (NonEmpty FormError) ()
    validateBuildTools = toError
        (not $ kit ^. cabal || kit ^. stack)
        CabalOrStack

    validateLibOrExe :: Validation (NonEmpty FormError) ()
    validateLibOrExe = toError
        (not $ kit ^. projectMeta . lib  || kit ^. projectMeta . exe)
        LibOrExe

-- | Returns list of error messages according to all invalid fields.
formErrorMessages :: [FilePath] -> KitForm e -> [String]
formErrorMessages dirs kitForm = validatedErrorMessages ++ ghcErrorMessage
  where
    validatedErrorMessages :: [String]
    validatedErrorMessages = case validateKit dirs (formState kitForm) of
        Success _      -> []
        Failure errors -> map showFormError $ toList errors

    -- Hack because input field for GHC versions uses custom @editField@ with its own validation
    ghcErrorMessage :: [String]
    ghcErrorMessage =
        ["Some GHC versions failed to parse: use space-separated valid GHC versions"
        | Ghcs `elem` invalidFields kitForm
        ]
