module Lib.ObservableV2 where

import Prelude
import Effect as Effect
import Data.Newtype as Newtype
import Data.Functor.Contravariant as Contravariant
import Lib.Observer as Observer
import Lib.Subscription as Subscription

newtype Observable a b = Observable (Unwrapped a b)

type Unwrapped a b = Observer.Observer a b -> Effect.Effect Subscription.Subscription

instance observableNewtype :: Newtype.Newtype (Observable a b) (Unwrapped a b)
instance observableFunctor :: Functor (Observable a) where
  map fn (Observable source) = Observable $ Contravariant.cmap fn >>> source
