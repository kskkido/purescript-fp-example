module Lib.SwitchObservable where

import Prelude
import Effect as Effect
import Effect.Ref as Effect.Ref
import Effect.Class as Effect.Class
import Effect.Console as Effect.Console
import Control.Alt as Alt
import Control.Alternative as Alternative
import Control.Applicative as Applicative
import Control.Plus as Plus
import Control.Monad.Cont.Trans as ContT
import Control.Monad.Maybe.Trans as MaybeT
import Web.Event.Event as Web.Event.Event
import Web.Event.EventTarget as Web.Event.EventTarget
import Web.Event.Internal.Types as Web.Event.Internal.Types
import Lib.Subscription as Subscription
import Lib.Observable as Observable
import Data.Traversable as Traversable
import Data.Newtype as Newtype
import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Data.Traversable as Traversable

newtype SwitchObservable a = SwitchObservable (Unwrapped a)

type Unwrapped a = Observable.Unwrapped a

instance switchObservableNewtype :: Newtype.Newtype (SwitchObservable a) (Unwrapped a)
instance switchObservableFunctor :: Functor (SwitchObservable) where
  map fn ma = fromObservable $ map fn (toObservable ma)
instance switchObservableApply :: Apply SwitchObservable where
  apply mfn ma = do
    fn <- mfn
    a <- ma
    pure $ fn a
instance switchObservableApplicative :: Applicative SwitchObservable where
  pure a = SwitchObservable $ \ca -> do
    ca a
    pure Subscription.empty
instance switchObservableSemigroup :: Semigroup (SwitchObservable a) where
  append ma mb = concat [ma, mb]
instance switchObservableMonoid :: Monoid (SwitchObservable a) where
  mempty = never
instance switchObservableAlt :: Alt.Alt SwitchObservable where
  alt ma mb = concat [ma, mb]
instance switchObservablePlus :: Plus.Plus SwitchObservable where
  empty = never
instance switchObservableAlternative :: Alternative.Alternative SwitchObservable
instance switchObservableMonadCont :: ContT.MonadCont SwitchObservable where
  callCC = call
instance switchObservableBind :: Bind SwitchObservable where
  bind ma fn = switchMap fn ma
instance switchObservableMonad :: Monad SwitchObservable
instance switchObservableMonadEffect :: Effect.Class.MonadEffect SwitchObservable where
  liftEffect ma = SwitchObservable $ \ca -> do
    a <- ma
    ca a
    pure Subscription.empty

call :: forall a b. ((a -> SwitchObservable b) -> SwitchObservable a) -> SwitchObservable a
call fn = SwitchObservable $ \ca -> do
  subscriptionRef <- Effect.Ref.new Subscription.empty
  subscription <- flip Newtype.unwrap ca $ fn $ \a -> do
    Effect.Class.liftEffect do
      subscription <- Effect.Ref.read subscriptionRef
      Subscription.unsubscribe subscription
    SwitchObservable \_ -> do
      ca a
      pure Subscription.empty
  Effect.Ref.write subscription subscriptionRef
  pure subscription

never :: forall a. SwitchObservable a
never = SwitchObservable \_ -> pure Subscription.empty

fromObservable :: forall a. Observable.Observable a -> SwitchObservable a
fromObservable = SwitchObservable <<< Newtype.unwrap

fromMaybe :: forall a. Maybe.Maybe a -> SwitchObservable a
fromMaybe ma = fromMaybeIO (MaybeT.MaybeT $ pure ma)

fromMaybeIO :: forall a. MaybeT.MaybeT Effect.Effect a -> SwitchObservable a
fromMaybeIO mma = SwitchObservable \ca -> do
  ma <- MaybeT.runMaybeT mma
  case ma of
    Maybe.Just a -> ca a
    Maybe.Nothing -> pure unit
  pure Subscription.empty

fromEvent :: Web.Event.Event.EventType -> Web.Event.EventTarget.EventTarget -> SwitchObservable Web.Event.Internal.Types.Event
fromEvent a b = fromObservable $ Observable.fromEvent a b

toObservable :: forall a. SwitchObservable a -> Observable.Observable a
toObservable = Observable.Observable <<< Newtype.unwrap

switch :: forall a. SwitchObservable (SwitchObservable a) -> SwitchObservable a
switch mma = SwitchObservable \ca -> do
  subscriptionRef <- Effect.Ref.new Subscription.empty
  subscriptionOuter <- Newtype.unwrap mma $ \ma -> do
    subscription <- Effect.Ref.read subscriptionRef
    Subscription.unsubscribe subscription
    subscriptionInner <- Newtype.unwrap ma ca
    void $ Effect.Ref.write subscriptionInner subscriptionRef
  pure $ Newtype.wrap do
    subscription <- Effect.Ref.read subscriptionRef
    Subscription.unsubscribe (subscriptionOuter <> subscription)

switchMap :: forall a b. (a -> SwitchObservable b) -> SwitchObservable a -> SwitchObservable b
switchMap fn ma = switch (fn <$> ma)

switchScan :: forall a b. (a -> b -> SwitchObservable b) -> b -> SwitchObservable a -> SwitchObservable b
switchScan fn seed ma = do
  currRef <- Effect.Class.liftEffect do
    Effect.Ref.new seed
  a <- ma
  curr <- Effect.Class.liftEffect do
    Effect.Ref.read currRef
  next <- fn a curr
  Effect.Class.liftEffect do
    Effect.Ref.write next currRef
  pure next

take :: forall a. Int -> SwitchObservable a -> SwitchObservable a
take n ma = call $ \unsubscribe -> do
  countRef <- Effect.Class.liftEffect do
    Effect.Ref.new 0
  a <- ma
  curr <- Effect.Class.liftEffect do
    Effect.Ref.read countRef
  Alternative.guard (curr < n)
  let next = curr + 1
  Effect.Class.liftEffect do
    Effect.Ref.write next countRef
  Applicative.when (next >= n) do
    unsubscribe a
  pure a

concat :: forall a. Array (SwitchObservable a) -> SwitchObservable a
concat mas = SwitchObservable \ca -> do
  ss <- Traversable.for mas \ma -> do
    Newtype.unwrap ma ca
  pure $ Subscription.concat ss

distinctWhile :: forall a. (a -> a -> Boolean) -> SwitchObservable a -> SwitchObservable a
distinctWhile fn ma = fromObservable (Observable.distinctWhile fn (toObservable ma))

distinctUntil :: forall a. (a -> a -> Boolean) -> SwitchObservable a -> SwitchObservable a
distinctUntil fn ma = fromObservable (Observable.distinctUntil fn (toObservable ma))

distinctUntilChanged :: forall a. Eq a => SwitchObservable a -> SwitchObservable a
distinctUntilChanged ma = fromObservable (Observable.distinctUntilChanged (toObservable ma))

combine :: forall a b. SwitchObservable a -> SwitchObservable b -> SwitchObservable (Tuple.Tuple a b)
combine ma mb = fromObservable $ Observable.combine (toObservable ma) (toObservable mb)

infixr 6 combine as /\

until :: forall t a. Traversable.Traversable t => t (SwitchObservable a) -> SwitchObservable Unit
until = fromObservable <<< Observable.until <<< (map toObservable)
