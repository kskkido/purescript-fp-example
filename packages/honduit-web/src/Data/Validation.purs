module Data.Validation where

import Prelude
import Control.Alt as Alt
import Control.Alternative as Alternative
import Control.Plus as Plus
import Data.Either as Either
import Data.Newtype as Newtype
import Data.Foldable as Foldable

newtype Validation a b = Validation (Unwrapped a b)

type Unwrapped a b = Either.Either a b

instance validationNewtype :: Newtype.Newtype (Validation a b) (Unwrapped a b)
instance validationShow :: (Show a, Show b) => Show (Validation a b) where
  show (Validation ma) = show ma
instance validationFunctor :: Functor (Validation a) where
  map fn ma = Newtype.wrap $ map fn (Newtype.unwrap ma)
instance validationApply :: Semigroup a => Apply (Validation a) where
  apply (Validation (Either.Left a)) (Validation (Either.Left c)) = Newtype.wrap $ Either.Left (a <> c)
  apply (Validation (Either.Left a)) _ = Newtype.wrap $ Either.Left a
  apply _ (Validation (Either.Left c)) = Newtype.wrap $ Either.Left c
  apply (Validation (Either.Right b)) (Validation (Either.Right d)) = Newtype.wrap $ Either.Right (b d)
instance validationApplicative :: Semigroup a => Applicative (Validation a) where
  pure = Newtype.wrap <<< pure
instance validationAlternative :: Monoid a => Alternative.Alternative (Validation a)
instance validationAlt :: Semigroup a => Alt.Alt (Validation a) where
  alt (Validation (Either.Left a)) (Validation (Either.Left c)) = Newtype.wrap $ Either.Left (a <> c)
  alt (Validation (Either.Left a)) y = y
  alt (Validation (Either.Right b)) _ = (Validation (Either.Right b))
instance validationSemigroup :: (Semigroup a, Semigroup b) => Semigroup (Validation a b) where
  append ma mb = append <$> ma <*> mb
instance validationMonoid :: (Semigroup a, Monoid b) => Monoid (Validation a b) where
  mempty = pure mempty
instance validationPlus :: Monoid a => Plus.Plus (Validation a) where
  empty = Newtype.wrap $ Either.Left mempty

fold :: forall a b c. (a -> c) -> (b -> c) -> Validation a b -> c
fold fn _ (Validation (Either.Left a))  = fn a
fold _ gn (Validation (Either.Right b)) = gn b

failure :: forall a b. a -> Validation a b
failure = Validation <<< Either.Left

success :: forall a b. b -> Validation a b
success = Validation <<< Either.Right
