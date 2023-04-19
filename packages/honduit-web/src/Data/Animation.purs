module Data.Animation where

import Prelude
import Effect as Effect
import Control.Monad.Reader as Reader
import Data.Array as Array
import Data.Tuple as Tuple
import Data.Int as Int
import Data.Range as Range
import Data.Traversable as Traversable
import Lib.Frp.Event as Frp.Event
import Lib.Frp.Behavior as Frp.Behavior

type Animation = Frp.Behavior.Behavior (Effect.Effect Unit)

merge :: forall a. Traversable.Traversable a => a Animation -> Animation
merge as = do
  es <- Traversable.sequence as
  pure $ Traversable.sequence_ es

reverse :: Animation -> Animation
reverse = Reader.withReader (\time -> 1.0 - time)

stagger :: Number -> Array Animation -> Animation
stagger total as =
  case Array.length as > 1 of
    true ->
      ( ( as ) #
        ( Array.mapWithIndex (\i a ->
            ( ( a ) #
              ( Frp.Behavior.between
                ( Tuple.Tuple
                  ( s * (Int.toNumber i) )
                  ( s * (Int.toNumber i) + (1.0 - total) )
                )
              )
            )
          )
        ) #
        ( merge )
      )
    _ ->
      merge as
  where s = total / (Int.toNumber $ Array.length as - 1)

