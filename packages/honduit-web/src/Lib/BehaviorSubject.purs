module Lib.BehaviorSubject where

import Prelude
import Effect as Effect
import Effect.Ref as Effect.Ref
import Data.Map as Map
import Data.Tuple as Tuple
import Data.Maybe as Maybe
import Data.Traversable as Traversable
import Control.Monad.Maybe.Trans as MaybeT
import Lib.Subscription as Subscription

type BehaviorSubject a =
  { notify :: (a -> Effect.Effect Unit)
  , listen :: (a -> Effect.Effect Unit) -> Effect.Effect Subscription.Subscription
  }

from :: forall a. a -> Effect.Effect (BehaviorSubject a)
from seed = do
  idRef <- Effect.Ref.new (0 :: Int)
  cbRef <- Effect.Ref.new Map.empty
  pure
    { notify: \next -> do
        cbs <- Effect.Ref.read cbRef
        Traversable.traverse_ ((#) next) cbs
    , listen: \cb -> do
        Effect.Ref.modify_ ((+) 1) idRef
        id <- Effect.Ref.read idRef
        cb seed
        Effect.Ref.modify_ (Map.insert id cb) cbRef
        pure $ Subscription.Subscription do
          Effect.Ref.modify_ (Map.delete id) cbRef
    }
