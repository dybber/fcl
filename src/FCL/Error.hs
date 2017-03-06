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

instance Pretty FCLError where
  pretty (ParseError err) = pretty err
  pretty (DesugarError err) = pretty err
  pretty (TypeError err) = pretty err
  pretty (MonomorphError err) = pretty err

instance Pretty ParseError where
  pretty err = text (show err)

instance Pretty DesugarError where
  pretty (DesugarLevelNotInScope lv) = text "Desugaring error. Level variable not in scope:" <+> pretty lv
  pretty (DesugarTyVarNotInScope tv) = text "Desugaring error. Type variable not in scope:" <+> pretty tv
  pretty DesugarEmptyDo = text "Desugaring error. Empty `do`-construct."
  pretty DesugarDoFinalExpIsBind = text "Desugaring error. Final expression in `do`-construct can not be a bind."

instance Pretty TypeError where
  pretty (UnificationError ty0 ty1) =
    text "Unification error." </>
      text "Cannot unify types:" <+> pretty ty0 <+> text "and" <+> pretty ty1
  pretty (UnexpectedPolymorphicVariable ident) =
    text "Unexpected polymorphic variable:" <+> pretty ident
  pretty (UnboundVariableError ident) = text "Variable:" <+> pretty ident <+> text "not defined."
  pretty (UnboundTypeVariableError ident) = text "Type variable:" <+> pretty ident <+> text "not defined."
  pretty (OccursCheckFailed tyvar ty) = text "Occurs check failed." <+> pretty tyvar <+> text "found in" <+> pretty ty
  pretty (LevelUnificationError l1 l2) =
    text "Unification error." </>
      text "Cannot unify levels: " <+> pretty l1 <+> text "and" <+> pretty l2
  pretty (LevelOccursCheckFailed lvlvar l) = text "Occurs check failed." <+> pretty lvlvar <+> text "found in" <+> pretty l
  pretty (NotFullyLevelApplied ident) = text "Variable" <+> pretty ident <+> text "is not applied to right number of level parameters."
  pretty (SignatureMismatch t1 t2) = text "Inferred type" <+> pretty t1 </> text "does not match signature type" <+> pretty t2

instance Pretty InstantiateError where
  pretty (TypeMismatch t1 t2) = text "Type mismatch during instantiation:" <+> pretty t1
                                <+> text "does not match" <+> pretty t2
  pretty (LevelMismatch l1 l2) = text "Level mismatch during instantiation:" <+> pretty l1
                                <+> text "does not match" <+> pretty l2

instance Pretty MonomorphError where
  pretty (NotInScope ident) = text "Identifier not in scope:" <+> pretty ident
  pretty (MonomorphInstantiateError err) = text "Monomorphization error." <+> pretty err
  pretty (UnexpectedPolyType t) = text "Unexpected polymorphic type " <+> pretty t <+> text "found during monomorphization."
  pretty (UnexpectedPolyLevel l) = text "Unexpected polymorphic level " <+> pretty l <+> text "found during monomorphization."
