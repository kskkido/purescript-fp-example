module Lib.Observer where

import Prelude
import Effect as Effect
import Data.Newtype as Newtype
import Data.Functor.Contravariant as Contravariant

newtype Observer a b = Observer (Unwrapped a b)

type Unwrapped a b =
  { next :: b -> Effect.Effect Unit
  , error :: a -> Effect.Effect Unit
  , complete :: b -> Effect.Effect Unit
  }

instance observerNewtype :: Newtype.Newtype (Observer a b) (Unwrapped a b)
instance observerContravariantFunctor :: Contravariant.Contravariant (Observer a) where
  cmap fn (Observer observer) = Observer
    { next: fn >>> observer.next
    , error: observer.error
    , complete: fn >>> observer.complete
    }
