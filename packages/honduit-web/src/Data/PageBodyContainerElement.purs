module Data.PageContainerElement where

import Prelude
import Effect as Effect
import Control.MonadZero as Control.MonadZero
import Control.Monad.Trans.Class as Trans
import Control.Monad.Maybe.Trans as MaybeT
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Number as Number
import Data.Foldable as Foldable
import Data.Traversable as Traversable
import Web.DOM.Element as Web.DOM.Element
import Web.DOM.NodeList as Web.DOM.NodeList
import Web.DOM.ParentNode as Web.DOM.ParentNode
import Web.DOM.DOMTokenList as Web.DOM.DOMTokenList
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument

type PageBodyContainerElement = Web.DOM.Element.Element

fromQueryMatch :: String -> Web.DOM.ParentNode.ParentNode -> Effect.Effect (Maybe.Maybe PageBodyContainerElement)
fromQueryMatch query parentNode = do
  let querySelector = Web.DOM.ParentNode.QuerySelector query
  Web.DOM.ParentNode.querySelector querySelector parentNode

fromQueryMatches :: String -> Web.DOM.ParentNode.ParentNode -> Effect.Effect (Array PageBodyContainerElement)
fromQueryMatches query parentNode = do
  let querySelector = Web.DOM.ParentNode.QuerySelector query
  Web.DOM.ParentNode.querySelectorAll querySelector parentNode >>= fromNodeList

fromNodeList :: Web.DOM.NodeList.NodeList -> Effect.Effect (Array PageBodyContainerElement)
fromNodeList nodeList = do
  Array.mapMaybe Web.DOM.Element.fromNode <$> Web.DOM.NodeList.toArray nodeList

