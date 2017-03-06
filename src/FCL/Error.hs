{-# OPTIONS_GHC -fno-warn-orphans #-}
module FCL.Error where

import Prelude hiding ((<$>))
import Text.PrettyPrint.Leijen

import FCL.Pretty ()
import FCL.Desugaring (DesugarError(..))
import FCL.Infer.Monad (TypeError(..))
import FCL.Instantiate (InstantiateError(..))
import FCL.Monomorphization (MonomorphError (..))
import FCL.External.Parser (ParseError)

data FCLError = ParseError ParseError
              | DesugarError DesugarError
              | TypeError TypeError
              | MonomorphError MonomorphError

ticks = enclose (char '`') (char '`')

instance Pretty FCLError where
  pretty (ParseError err)     = pretty err
  pretty (DesugarError err)   = pretty err
  pretty (TypeError err)      = pretty err
  pretty (MonomorphError err) = pretty err

instance Pretty ParseError where
  pretty err = text (show err)

instance Pretty DesugarError where
  pretty (DesugarLevelNotInScope lv) = text "Level variable not in scope:" <+> ticks (pretty lv)
  pretty (DesugarTyVarNotInScope tv) = text "Type variable not in scope:" <+> ticks (pretty tv)
  pretty DesugarEmptyDo              = text "Empty `do`-construct."
  pretty DesugarDoFinalExpIsBind     = text "Final expression in `do`-construct can not be a bind."

instance Pretty TypeError where
  pretty (UnificationError ty0 ty1) =
    text "Cannot unify:" <+> ticks (pretty ty0) <+> text "and" <+> ticks (pretty ty1)
  pretty (UnexpectedPolymorphicVariable ident) =
    text "Unexpected polymorphic variable:" <+> pretty ident
  pretty (UnboundVariableError ident) =
    text "Not in scope:" <+> ticks (pretty ident)
  pretty (UnboundTypeVariableError ident) =
    text "Not in scope: type variable" <+> ticks (pretty ident)
  pretty (OccursCheckFailed tyvar ty) = text "Occurs check failed." <+> ticks (pretty tyvar) <+> text "found in" <+> ticks (pretty ty)
  pretty (LevelUnificationError l1 l2) =
      text "Cannot unify levels: " <+> ticks (pretty l1) <+> text "and" <+> ticks (pretty l2)
  pretty (LevelOccursCheckFailed lvlvar l) = text "Occurs check failed." <+> ticks (pretty lvlvar) <+> text "found in" <+> ticks (pretty l)
  pretty (NotFullyLevelApplied ident) = text "Variable" <+> ticks (pretty ident) <+> text "is not applied to right number of level parameters."
  pretty (SignatureMismatch t1 t2) = text "Inferred type" <+> ticks (pretty t1) </> text "does not match signature type" <+> pretty t2

instance Pretty InstantiateError where
  pretty (TypeMismatch t1 t2) = text "Type mismatch during instantiation:" <+> ticks (pretty t1)
                                <+> text "does not match" <+> ticks (pretty t2)
  pretty (LevelMismatch l1 l2) = text "Level mismatch during instantiation:" <+> ticks (pretty l1)
                                <+> text "does not match" <+> ticks (pretty l2)

instance Pretty MonomorphError where
  pretty (NotInScope ident)              = text "Not in scope:" <+> ticks (pretty ident)
  pretty (MonomorphInstantiateError err) = pretty err
  pretty (UnexpectedPolyType t)          = text "Unexpected polymorphic type " <+> ticks (pretty t) <+> text "found during monomorphization."
  pretty (UnexpectedPolyLevel l)         = text "Unexpected polymorphic level " <+> ticks (pretty l) <+> text "found during monomorphization."
