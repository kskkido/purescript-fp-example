module Data.DomAnimation where

import Prelude
import Effect.Console as Effect.Console
import Control.Monad.Reader as Reader
import Control.Monad.Maybe.Trans as MaybeT
import Web.DOM.Element as Element
import Web.HTML.HTMLElement as HTMLElement
import Lib.Dom.HtmlElement as Lib.Dom.HtmlElement
import Data.Maybe as Maybe
import Data.Animation as Animation

fadeIn :: HTMLElement.HTMLElement -> Animation.Animation
fadeIn htmlElement = do
  time <- Reader.ask
  pure $ void do
    flip Lib.Dom.HtmlElement.setStyles htmlElement $
      { opacity:
        ( ""
          <> show (1.0 * time)
        )
      }

fadeOut :: HTMLElement.HTMLElement -> Animation.Animation
fadeOut = Animation.reverse <<< fadeIn

slideInBottom :: HTMLElement.HTMLElement -> Animation.Animation
slideInBottom htmlElement = do
  time <- Reader.ask
  pure $ void do
    flip Lib.Dom.HtmlElement.setStyles htmlElement $
      { transform:
        ( ""
          <> "translateY("
          <> show (100.0 - 100.0 * time)
          <> "%)"
        )
      }

slideOutBottom :: HTMLElement.HTMLElement -> Animation.Animation
slideOutBottom = Animation.reverse <<< slideInBottom

slideInBottomPx :: Number -> HTMLElement.HTMLElement -> Animation.Animation
slideInBottomPx px htmlElement = do
  time <- Reader.ask
  pure $ void do
    flip Lib.Dom.HtmlElement.setStyles htmlElement $
      { transform:
        ( ""
          <> "translateY("
          <> show (px - px * time)
          <> "px)"
        )
      }

slideOutBottomPx :: Number -> HTMLElement.HTMLElement -> Animation.Animation
slideOutBottomPx px = Animation.reverse <<< slideInBottomPx px
