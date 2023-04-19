module Lib.Either where

import Prelude
import Effect as Effect
import Effect.Exception as Exception
import Data.Either as Either

toIO :: forall a. Either.Either Exception.Error a -> Effect.Effect a
toIO (Either.Left  x) = Exception.throwException x
toIO (Either.Right x) = pure x
