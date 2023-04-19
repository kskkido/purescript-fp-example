module Lib.SwitchObservableV2 where

import Prelude
import Effect.Ref as Effect.Ref
import Data.Newtype as Newtype
import Lib.Subscription as Subscription
import Lib.ObservableV2 as Observable

newtype SwitchObservable a b = SwitchObservable (Unwrapped a b)

type Unwrapped a b = Observable.Unwrapped a b

instance switchObservableNewtype :: Newtype.Newtype (SwitchObservable a b) (Unwrapped a b)
instance switchObservableFunctor :: Functor (SwitchObservable a) where
  map fn ma = fromObservable $ map fn (toObservable ma)

fromObservable :: forall a b. Observable.Observable a b -> SwitchObservable a b
fromObservable = SwitchObservable <<< Newtype.unwrap

toObservable :: forall a b. SwitchObservable a b -> Observable.Observable a b
toObservable = Observable.Observable <<< Newtype.unwrap

join :: forall a b. SwitchObservable a (SwitchObservable a b) -> SwitchObservable a b
join mma = SwitchObservable \observer -> do
  subscriptionRef <- Effect.Ref.new Subscription.empty
  subscriptionOuter <- Newtype.unwrap mma $ Newtype.wrap
    { next: \ma -> do
        subscription <- Effect.Ref.read subscriptionRef
        Subscription.unsubscribe subscription
        subscriptionInner <- Newtype.unwrap ma observer
        Effect.Ref.write subscriptionInner subscriptionRef
    , complete: \ma -> do
        subscription <- Effect.Ref.read subscriptionRef
        Subscription.unsubscribe subscription
        subscriptionInner <- Newtype.unwrap ma observer
        Effect.Ref.write subscriptionInner subscriptionRef
    , error: (Newtype.unwrap observer).error
    }
  Effect.Ref.write subscriptionOuter subscriptionRef
  pure $ Newtype.wrap do
    subscription <- Effect.Ref.read subscriptionRef
    Subscription.unsubscribe (subscription <> subscriptionOuter)
