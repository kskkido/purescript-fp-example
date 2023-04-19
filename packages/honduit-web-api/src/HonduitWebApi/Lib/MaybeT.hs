module HonduitWebApi.Lib.MaybeT
  ( fromMaybe
  ) where

import RIO hiding (fromMaybe)
import qualified Control.Monad.Trans.Maybe as Maybe
import qualified Data.Maybe as Maybe

fromMaybe :: Monad m => Maybe.Maybe a -> Maybe.MaybeT m a
fromMaybe = Maybe.MaybeT . pure

