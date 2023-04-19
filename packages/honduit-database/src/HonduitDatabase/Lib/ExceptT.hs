module HonduitDatabase.Lib.ExceptT
  ( liftFail
  , fromMaybeT
  ) where

import RIO
import qualified Control.Monad.Fail as Monad.Fail
import qualified Control.Monad.Except as Except
import qualified Control.Monad.Trans.Maybe as Maybe

liftFail :: Monad.Fail.MonadFail m => Except.ExceptT String m a -> m a
liftFail ma = do
  ea <- Except.runExceptT ma
  either Monad.Fail.fail pure ea

fromMaybeT :: Monad m => b -> Maybe.MaybeT m a -> Except.ExceptT b m a
fromMaybeT b mma = Except.ExceptT do
  ma <- Maybe.runMaybeT mma
  pure $ maybe (Left b) Right ma

