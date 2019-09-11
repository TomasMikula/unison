{-# Language DeriveFunctor #-}
{-# Language DeriveTraversable #-}

module Unison.Typechecker.TypeVar where

import qualified Data.Set as Set
import qualified Unison.ABT as ABT
import qualified Unison.Term as Term
import           Unison.Term (AnnotatedTerm)
import           Unison.Type (Type)
import           Unison.Var (Var)
import qualified Unison.Var as Var

data TypeVar b v = Universal v | Existential b v deriving (Functor, Foldable, Traversable)

instance Eq v => Eq (TypeVar b v) where
  Universal v == Universal v2 = v == v2
  Existential _ v == Existential _ v2 = v == v2
  _ == _ = False

instance Ord v => Ord (TypeVar b v) where
  Universal v `compare` Universal v2 = compare v v2
  Existential _ v `compare` Existential _ v2 = compare v v2
  Universal _ `compare` Existential _ _ = LT
  _ `compare` _ = GT

underlying :: TypeVar b v -> v
underlying (Universal v) = v
underlying (Existential _ v) = v

instance Show v => Show (TypeVar b v) where
  show (Universal v) = show v
  show (Existential _ v) = "'" ++ show v

instance ABT.Var v => ABT.Var (TypeVar b v) where
  freshIn s v = ABT.freshIn (Set.map underlying s) <$> v

instance Var v => Var (TypeVar b v) where
  named n = Universal (Var.named n)
  name v = Var.name (underlying v)
  reset v = Var.reset <$> v

liftType :: Ord v => Type v a -> Type (TypeVar b v) a
liftType = ABT.vmap Universal

lowerType :: Ord v => Type (TypeVar b v) a -> Type v a
lowerType = ABT.vmap underlying

liftTerm :: Ord v => AnnotatedTerm v a -> AnnotatedTerm (TypeVar b v) a
liftTerm = Term.vmap Universal

lowerTerm :: Ord v => AnnotatedTerm (TypeVar b v) a -> AnnotatedTerm v a
lowerTerm = Term.vmap underlying