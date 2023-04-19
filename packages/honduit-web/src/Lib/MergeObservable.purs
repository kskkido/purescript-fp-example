module Lib.MergeObservable where

import Prelude
import Effect.Ref as Effect.Ref
import Effect.Class as Effect.Class
import Effect.Console as Effect.Console
import Control.Applicative as Applicative
import Control.Monad.Cont.Trans as ContT
import Data.Newtype as Newtype
import Data.Tuple as Tuple
import Data.Tuple.Nested as Tuple
import Lib.Subscription as Subscription
import Lib.Observable as Observable

newtype MergeObservable a = MergeObservable (Unwrapped a)

type Unwrapped a = Observable.Unwrapped a

instance mergeObservableNewtype :: Newtype.Newtype (MergeObservable a) (Unwrapped a)
instance mergeObservableFunctor :: Functor (MergeObservable) where
  map fn ma = fromObservable $ map fn (toObservable ma)
instance mergeObservableApply :: Apply MergeObservable where
  apply mfn ma = do
    fn <- mfn
    a <- ma
    pure $ fn a
instance mergeObservableApplicative :: Applicative MergeObservable where
  pure a = MergeObservable $ \ca -> do
    ca a
    pure Subscription.empty
instance mergeObservableBind :: Bind MergeObservable where
  bind ma fn = mergeMap fn ma
instance mergeObservableMonadCont :: ContT.MonadCont MergeObservable where
  callCC = call
instance mergeObservableMonad :: Monad MergeObservable
instance mergeObservableMonadEffect :: Effect.Class.MonadEffect MergeObservable where
  liftEffect ma = MergeObservable $ \ca -> do
    a <- ma
    ca a
    pure Subscription.empty

call :: forall a b. ((a -> MergeObservable b) -> MergeObservable a) -> MergeObservable a
call fn = MergeObservable $ \ca -> do
  saRef <- Effect.Ref.new Subscription.empty
  sa <- flip Newtype.unwrap ca $ fn $ \a -> do
    Effect.Class.liftEffect $ do
      sa <- Effect.Ref.read saRef
      Subscription.unsubscribe sa
    MergeObservable $ \_ -> do
      ca a
      pure Subscription.empty
  Effect.Ref.write sa saRef
  pure sa

fromObservable :: forall a. Observable.Observable a -> MergeObservable a
fromObservable = MergeObservable <<< Newtype.unwrap

toObservable :: forall a. MergeObservable a -> Observable.Observable a
toObservable = Observable.Observable <<< Newtype.unwrap

never :: forall a. MergeObservable a
never = fromObservable Observable.never

merge :: forall a. MergeObservable (MergeObservable a) -> MergeObservable a
merge mma = MergeObservable \ca -> do
  ref <- Effect.Ref.new Subscription.empty
  sa <- Newtype.unwrap mma $ \ma -> do
    next <- Newtype.unwrap ma ca
    void $ Effect.Ref.modify ((<>) next) ref
  pure $ Newtype.wrap do
    sb <- Effect.Ref.read ref
    Subscription.unsubscribe (sa <> sb)

mergeMap :: forall a b. (a -> MergeObservable b) -> MergeObservable a -> MergeObservable b
mergeMap fn ma = merge (fn <$> ma)

take :: forall a. Int -> MergeObservable a -> MergeObservable a
take n ma = call $ \unsubscribe -> do
  countRef <- Effect.Class.liftEffect do
    Effect.Ref.new 0
  a <- ma
  count <- Effect.Class.liftEffect do
    Effect.Ref.modify ((+) 1) countRef
  Effect.Class.liftEffect do
    Effect.Console.log $ show count
  Applicative.when (count >= n) do
    Effect.Class.liftEffect do
      Effect.Console.log "done???"
    unsubscribe a
  pure a

combine :: forall a b. MergeObservable a -> MergeObservable b -> MergeObservable (Tuple.Tuple a b)
combine ma mb = fromObservable $ Observable.combine (toObservable ma) (toObservable mb)

infixr 6 combine as /\

groupBy :: forall a b. Ord b => (a -> b) -> MergeObservable a -> MergeObservable (Tuple.Tuple b (Observable.Observable a))
groupBy fn ma = fromObservable $ Observable.groupBy fn (toObservable ma)
