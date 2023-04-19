module Lib.List where

import Prelude
import Data.Maybe as Maybe
import Data.List as List
import Data.Tuple as Tuple

splitAt :: forall a. Eq a => (a -> Boolean) -> List.List a -> List.List (List.List a)
splitAt fn List.Nil = List.Nil
splitAt fn xs = Tuple.fst tx List.: splitAt fn (Tuple.snd tx)
  where tx = lookaround fn xs

lookaround :: forall a. Eq a => (a -> Boolean) -> List.List a -> Tuple.Tuple (List.List a) (List.List a)
lookaround fn xs = Tuple.Tuple spanned.init (Maybe.fromMaybe List.Nil $ List.tail spanned.rest)
  where spanned = List.span fn xs
