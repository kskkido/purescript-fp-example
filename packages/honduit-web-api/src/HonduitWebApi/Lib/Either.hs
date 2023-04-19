module HonduitWebApi.Lib.Either
  ( toMonadFail
  ) where

import RIO

toMonadFail :: (MonadFail m) => (a -> m b) -> m (Either a b) -> m b
toMonadFail fn ma = do
  a <- ma
  either fn pure a
