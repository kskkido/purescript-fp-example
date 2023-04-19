module Main where

import Prelude
import Effect as Effect
import Effect.Class as Effect.Class
import Control.Monad.Maybe.Trans as MaybeT
import Control.Monad.Reader as Reader
import Web.HTML as Web.HTML
import Data.AppMainContext as AppMainContext
import Services.App as App

main :: Effect.Effect Unit
main = do
  window <- Web.HTML.window
  void $ MaybeT.runMaybeT do
    context <- AppMainContext.fromWindow window
    Effect.Class.liftEffect $ do
      Reader.runReaderT App.main context

