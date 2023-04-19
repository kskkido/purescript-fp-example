module Data.Validator where

import Prelude
import Effect.Exception as Exception
import Control.Alt as Alt
import Control.Alternative as Alternative
import Control.Plus as Plus
import Lib.Plus as Plus
import Data.Map as Map
import Data.Newtype as Newtype
import Data.List as List
import Data.Foldable as Foldable
import Data.Validation as Validation
import Data.Route as Route

newtype Validator a b c = Validator (Unwrapped a b c)

type Unwrapped a b c = a -> Validation.Validation b c

instance validatorNewtype :: Newtype.Newtype (Validator a b c) (Unwrapped a b c)
instance validatorFunctor :: Functor (Validator a b) where
  map fn ma = Newtype.wrap $ map (map fn) (Newtype.unwrap ma)
instance validatorAlt :: Semigroup b => Alt.Alt (Validator a b) where
  alt (Validator ma) (Validator mb) = Validator $ \x -> ma x `Alt.alt` mb x
instance validatorSemigroup :: (Semigroup b, Semigroup c) => Semigroup (Validator a b c) where
  append (Validator ma) (Validator mb) = Validator $ \x -> ma x <> mb x
instance validatorPlus :: (Monoid b) => Plus.Plus (Validator a b) where
  empty = Validator $ \_ -> Plus.empty

run :: forall a b c. a -> Validator a b c -> Validation.Validation b c
run a (Validator ma) = ma a

sum :: forall a b c f. Monoid b => Foldable.Foldable f => f (Validator a b c) -> Validator a b c
sum = Plus.sum

fold :: forall a b c d. (b -> d) -> (c -> d) -> Validator a b c -> a -> d
fold fn gn (Validator ma) x = Validation.fold fn gn (ma x)
