module Lib.Observable where

import Prelude
import Effect as Effect
import Effect.Aff as Effect.Aff
import Effect.Now as Effect.Now
import Effect.Class as Effect.Class
import Effect.Ref as Effect.Ref
import Data.Array as Array
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Newtype as Newtype
import Data.Foldable as Foldable
import Data.Traversable as Traversable
import Data.Tuple as Tuple
import Data.Tuple.Nested as Tuple
import Data.DateTime.Instant as Instant
import Control.Alt as Alt
import Control.Alternative as Alternative
import Control.Applicative as Applicative
import Control.Monad.Trans.Class as Trans
import Control.Monad.Cont.Trans as ContT
import Control.Monad.Maybe.Trans as MaybeT
import Control.MonadZero as Control.MonadZero
import Control.Plus as Plus
import Web.Event.Event as Web.Event.Event
import Web.Event.EventTarget as Web.Event.EventTarget
import Web.Event.Internal.Types as Web.Event.Internal.Types
import Web.HTML.Window as Web.HTML.Window
import Lib.Window as Window
import Lib.Subject as Subject
import Lib.Subscription as Subscription

-- (a -> Effect.Effect ()) -> Effect.Effect ()
newtype Observable a = Observable (Unwrapped a)

type Unwrapped a = (a -> Effect.Effect Unit) -> Effect.Effect Subscription.Subscription

instance observableNewtype :: Newtype.Newtype (Observable a) (Unwrapped a)
instance observableFunctor :: Functor Observable where
  map fn ma = Observable $ \cb -> do
    Newtype.unwrap ma $ \a -> cb (fn a)
instance observableApply :: Apply Observable where
  apply mfn ma = do
    fn <- mfn
    a <- ma
    pure $ fn a
instance observableApplicative :: Applicative Observable where
  pure a = Observable $ \ca -> do
    ca a
    pure Subscription.empty
instance observableAlt :: Alt.Alt Observable where
  alt ma mb = concat [ma, mb]
instance observableAlternative :: Alternative.Alternative Observable
instance observableBind :: Bind Observable where
  bind ma fn = switchMap fn ma
instance observableMonad :: Monad Observable
instance observableMonadEffect :: Effect.Class.MonadEffect Observable where
  liftEffect ma = Observable $ \ca -> do
    a <- ma
    ca a
    pure Subscription.empty
instance observableMonadCont :: ContT.MonadCont Observable where
  callCC = call
instance observableSemigroup :: Semigroup (Observable a) where
  append mx my = concat [mx, my]
instance observableMonoid :: Monoid (Observable a) where
  mempty = never
instance observablePlus :: Plus.Plus Observable where
  empty = never

from :: forall a. Unwrapped a -> Observable a
from = Newtype.wrap

subscribe :: forall a. Observable a -> (a -> Effect.Effect Unit) -> Effect.Effect Subscription.Subscription
subscribe = Newtype.unwrap

call :: forall a b. ((a -> Observable b) -> Observable a) -> Observable a
call fn = Observable $ \ca -> do
  saRef <- Effect.Ref.new Subscription.empty
  sa <- flip Newtype.unwrap ca $ fn $ \a -> do
    Effect.Class.liftEffect $ do
      sa <- Effect.Ref.read saRef
      Subscription.unsubscribe sa
    Observable $ \_ -> do
      ca a
      pure Subscription.empty
  Effect.Ref.write sa saRef
  pure sa

switch :: forall a. Observable (Observable a) -> Observable a
switch mma = Observable $ \ca -> do
  ref <- Effect.Ref.new Subscription.empty
  sa <- Newtype.unwrap mma $ \ma -> do
    prev <- Effect.Ref.read ref
    Subscription.unsubscribe prev
    next <- Newtype.unwrap ma ca
    Effect.Ref.write next ref
  pure $ Newtype.wrap do
    sb <- Effect.Ref.read ref
    Subscription.unsubscribe (sa <> sb)

switchMap :: forall a b. (a -> Observable b) -> Observable a -> Observable b
switchMap fn ma = switch $ fn <$> ma

merge :: forall a. Observable (Observable a) -> Observable a
merge mma = Observable $ \ca -> do
  ref <- Effect.Ref.new Subscription.empty
  sa <- Newtype.unwrap mma $ \ma -> do
    next <- Newtype.unwrap ma ca
    void $ Effect.Ref.modify ((<>) next) ref
  pure $ Newtype.wrap do
    sb <- Effect.Ref.read ref
    Subscription.unsubscribe (sa <> sb)

mergeMap :: forall a b. (a -> Observable b) -> Observable a -> Observable b
mergeMap fn ma = merge $ fn <$> ma

replace :: forall a. Observable (Observable a) -> Observable a
replace = merge <<< take 1

replaceMap :: forall a b. (a -> Observable b) -> Observable a -> Observable b
replaceMap fn ma = replace $ fn <$> ma

toCont :: forall a. Observable a -> ContT.ContT Unit Effect.Effect a
toCont ma = ContT.ContT $ \ca -> do
  void $ Newtype.unwrap ma ca

fromCont :: forall a. ContT.ContT Unit Effect.Effect a -> Observable a
fromCont ma = Observable $ \ca -> do
  _ <- ContT.runContT ma ca
  pure Subscription.empty

fromEvent :: Web.Event.Event.EventType -> Web.Event.EventTarget.EventTarget -> Observable Web.Event.Internal.Types.Event
fromEvent eventType target = Observable $ \ce -> do
  listener <- Web.Event.EventTarget.eventListener ce
  Web.Event.EventTarget.addEventListener eventType listener false target
  pure $ Newtype.wrap do
    Web.Event.EventTarget.removeEventListener eventType listener false target

fromAnimationLoop :: Web.HTML.Window.Window -> Observable Number
fromAnimationLoop window = Observable $ \ca -> do
  Newtype.wrap <$> Window.requestAnimationLoop ca window

fromAff :: forall a. Effect.Aff.Aff a -> Observable a
fromAff ax = Observable $ \ca -> do
  Effect.Aff.launchAff_ do
    a <- ax
    Effect.Class.liftEffect (ca a)
  pure $ Subscription.empty

singleton :: forall a. Observable a -> Effect.Effect (Observable a)
singleton ma = do
  prevRef <- Effect.Ref.new Maybe.Nothing
  subject <- Subject.from
  _ <- subject.listen $ \a -> do
    Effect.Ref.write (pure a) prevRef
  _ <- subscribe ma subject.notify
  pure $ Observable \ca -> do
    prev <- Effect.Ref.read prevRef
    void $ case prev of
      Maybe.Just a -> do
        ca a
      Maybe.Nothing -> pure unit
    subject.listen ca

withLatestFrom :: forall a b. Observable b -> Observable a -> Observable (Tuple.Tuple a b)
withLatestFrom mb ma = Observable $ \cab -> do
  bRef <- Effect.Ref.new Maybe.Nothing
  sa <- Newtype.unwrap ma $ \a -> do
    void $ MaybeT.runMaybeT do
      b <- MaybeT.MaybeT $ Effect.Ref.read bRef
      Trans.lift $ cab (Tuple.Tuple a b)
  sb <- Newtype.unwrap mb $ \b -> do
    Effect.Ref.write (pure b) bRef
  pure $ sa <> sb

combine :: forall a b. Observable a -> Observable b -> Observable (Tuple.Tuple a b)
combine ma mb = Observable $ \cab -> do
  aRef <- Effect.Ref.new Maybe.Nothing
  bRef <- Effect.Ref.new Maybe.Nothing
  sa <- Newtype.unwrap ma $ \a -> do
    Effect.Ref.write (pure a) aRef
    void $ MaybeT.runMaybeT do
      b <- MaybeT.MaybeT $ Effect.Ref.read bRef
      Trans.lift $ cab (Tuple.Tuple a b)
  sb <- Newtype.unwrap mb $ \b -> do
    Effect.Ref.write (pure b) bRef
    void $ MaybeT.runMaybeT do
      a <- MaybeT.MaybeT $ Effect.Ref.read aRef
      Trans.lift $ cab (Tuple.Tuple a b)
  pure $ sa <> sb

cons :: forall a. Observable a -> Observable (Array a) -> Observable (Array a)
cons ma mas =
  ( combine ma mas ) #
  ( map \(a Tuple./\ as) -> a `Array.cons` as )

infixr 6 combine as /\

scan :: forall a b. (a -> b -> b) -> b -> Observable a -> Observable b
scan step seed ma =
  concat
    [ do
        ref <- Effect.Class.liftEffect (Effect.Ref.new seed)
        a <- ma
        Observable $ \cb -> do
          acc <- Effect.Ref.modify (step a) ref
          cb acc
          pure Subscription.empty
    , pure seed
    ]

switchScan :: forall a b. (a -> b -> Observable b) -> b -> Observable a -> Observable b
switchScan step seed ma = Observable \cb -> do
  ref <- Effect.Ref.new seed
  Newtype.unwrap
    ( ( ma ) #
      ( switchMap \a -> Observable \cb -> do
          curr <- Effect.Ref.read ref
          Newtype.unwrap (step a curr) cb
      ) #
      ( tapIO \next -> do
          Effect.Ref.write next ref
      )
    )
    cb

startWith :: forall a. a -> Observable a -> Observable a
startWith a oa =
  concat
    [ oa
    , pure a
    ]

loop :: forall a. (a -> Observable a) -> a -> Observable a
loop step seed = Observable $ \ca -> do
  flip Newtype.unwrap ca $ do
    let iter x = replaceMap iter (tapIO ca $ step x)
    iter seed

expand :: forall a. (a -> Observable a) -> Observable a -> Observable a
expand step ma = Observable $ \ca -> do
  flip Newtype.unwrap ca $ do
    let iter x = replaceMap iter (tapIO ca $ step x)
    switchMap iter (tapIO ca ma)

take :: forall a. Int -> Observable a -> Observable a
take n ma = call $ \unsubscribe -> do
  countRef <- Effect.Class.liftEffect $ Effect.Ref.new 0
  a <- ma
  count <- Effect.Class.liftEffect $ Effect.Ref.modify ((+) 1) countRef
  Applicative.when (count >= n) (unsubscribe a)
  pure a

takeWhile :: forall a. (a -> Boolean) -> a -> Observable a -> Observable a
takeWhile pred terminal ma = call $ \unsubscribe -> do
  a <- ma
  Applicative.when (not $ pred a) (unsubscribe terminal)
  pure a

distinctUntil :: forall a. (a -> a -> Boolean) -> Observable a -> Observable a
distinctUntil pred ma = Observable \ca -> do
  prevRef <- Effect.Ref.new Maybe.Nothing
  Newtype.unwrap ma \a -> do
    prev <- Effect.Ref.read prevRef
    case prev of
      Maybe.Just b -> do
        Applicative.when (pred a b) (ca a)
      _ -> do
        ca a
    Effect.Ref.write (pure a) prevRef

distinctUntilChanged :: forall a. Eq a => Observable a -> Observable a
distinctUntilChanged = distinctUntil (\curr prev -> curr /= prev)

distinctWhile :: forall a. (a -> a -> Boolean) -> Observable a -> Observable a
distinctWhile pred = distinctUntil (\a b -> not $ pred a b)

concat :: forall a. Array (Observable a) -> Observable a
concat mas = Observable $ \ca -> do
  ss <- Traversable.for mas $ \ma -> do
    Newtype.unwrap ma ca
  pure $ Subscription.concat ss

filter :: forall a. (a -> Boolean) -> Observable a -> Observable a
filter fn ma = Observable $ \ca -> do
  Newtype.unwrap ma $ \a -> do
    Applicative.when (fn a) (ca a)

groupBy :: forall a b. Ord b => (a -> b) -> Observable a -> Observable (Tuple.Tuple b (Observable a))
groupBy fn ma = Observable $ \cab -> do
  currRef <- Effect.Ref.new (Map.empty :: Map.Map b (Subject.Subject a))
  Newtype.unwrap ma $ \a -> do
    let key = fn a
    curr <- Effect.Ref.read currRef
    group <- case Map.lookup key curr of
      Maybe.Just group -> do
        pure group
      _ -> do
        group <- Subject.from
        cab (Tuple.Tuple key $ from group.listen)
        Effect.Ref.modify_ (Map.insert key group) currRef
        pure group
    group.notify a

replay :: forall a. Observable a -> Effect.Effect (Observable a)
replay ma = do
  prevRef <- Effect.Ref.new Maybe.Nothing
  pure $ Observable \ca -> do
    prev <- Effect.Ref.read prevRef
    void $ case prev of
      Maybe.Just a -> do
        ca a
      Maybe.Nothing -> pure unit
    Newtype.unwrap ma $ \a -> do
      ca a
      Effect.Ref.write (pure a) prevRef

until :: forall t a. Foldable.Foldable t => t (Observable a) -> Observable Unit
until mas =
  ( ( Foldable.foldr cons (pure []) mas ) #
    ( map $ const unit )
  )

filterMap :: forall a b. (a -> Maybe.Maybe b) -> Observable a -> Observable b
filterMap fn ma = Observable $ \cb -> do
  Newtype.unwrap (fn <$> ma) $ Maybe.maybe (pure unit) cb

filterMapIO :: forall a b. (a -> MaybeT.MaybeT Effect.Effect b) -> Observable a -> Observable b
filterMapIO fn ma =
  ( ma ) #
  ( bindIO $ MaybeT.runMaybeT <<< fn ) #
  ( filterMap identity )

fromMaybe :: forall a. Maybe.Maybe a -> Observable a
fromMaybe ma = Observable $ \ca -> do
  void $ case ma of
    Maybe.Just a -> ca a
    Maybe.Nothing -> pure unit
  pure $ Subscription.empty

fromMaybeIO :: forall a. MaybeT.MaybeT Effect.Effect a -> Observable a
fromMaybeIO mea = Observable $ \ca -> do
  ma <- MaybeT.runMaybeT mea
  (Newtype.unwrap $ fromMaybe ma) ca

never :: forall a. Observable a
never = Observable \_ -> pure Subscription.empty

when :: forall a. Boolean -> Observable a -> Observable a
when bx mx = filter (const bx) mx

compact :: forall a. Observable (Maybe.Maybe a) -> Observable a
compact = filterMap identity

bindIO :: forall a b. (a -> Effect.Effect b) -> Observable a -> Observable b
bindIO fn mx = joinIO $ fn <$> mx

joinIO :: forall a. Observable (Effect.Effect a) -> Observable a
joinIO mea = mea >>= Effect.Class.liftEffect

tapIO :: forall a. (a -> Effect.Effect Unit) -> Observable a -> Observable a
tapIO fn ma = Observable $ \ca -> do
  Newtype.unwrap ma $ \a -> do
    fn a
    ca a
