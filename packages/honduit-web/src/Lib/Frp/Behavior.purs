module Lib.Frp.Behavior where

import Prelude
import Control.Monad.Reader as Reader
import Data.Tuple as Tuple
import Data.Range as Range
import Lib.Frp.Event as Event

type Behavior a = Reader.Reader Number a

at :: forall a. Behavior a -> Number -> a
at ba t = Reader.runReader ba t

time :: Behavior Number
time = Reader.ask

until :: forall a. Event.Event (Behavior a) -> Behavior a -> Behavior a
until eba ba = do
  time <- Reader.ask
  case time < eba.time of
    false -> eba.value
    true -> ba

after :: forall a. Event.Event (Behavior a) -> Behavior a -> Behavior a
after eba ba = do
  time <- Reader.ask
  case time > eba.time of
    false -> eba.value
    true -> ba

transform :: forall a. Behavior Number -> Behavior a -> Behavior a
transform bt ba = at ba <$> bt

snapshot :: forall a b. Event.Event a -> Behavior b -> Event.Event (Tuple.Tuple a b)
snapshot ea bb =
  { time: ea.time
  , value: Tuple.Tuple ea.value (at bb ea.time)
  }

between :: forall a. Range.Range -> Behavior a -> Behavior a
between ra ba =
  ( ( ba ) #
    ( Reader.withReader
      (\time -> (time - Tuple.fst ra) / Range.distance ra)
    ) #
    ( after
        { time: Tuple.fst ra
        , value: pure $ at ba (Tuple.fst ra)
        }
    ) #
    ( until
        { time: Tuple.snd ra
        , value: pure $ at ba (Tuple.snd ra)
        }
    )
  )
