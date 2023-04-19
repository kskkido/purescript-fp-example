module Lib.Dom.Element where

import Prelude
import Effect as Effect
import Effect.Ref as Effect.Ref
import Control.MonadZero as Control.MonadZero
import Control.Monad.Trans.Class as Trans
import Control.Monad.Maybe.Trans as MaybeT
import Data.Array as Array
import Data.Maybe as Maybe
import Data.Int as Int
import Data.Number as Number
import Data.Foldable as Foldable
import Data.Traversable as Traversable
import Web.DOM.Element as Web.DOM.Element
import Web.DOM.NodeList as Web.DOM.NodeList
import Web.DOM.ChildNode as Web.DOM.ChildNode
import Web.DOM.ParentNode as Web.DOM.ParentNode
import Web.DOM.DOMTokenList as Web.DOM.DOMTokenList
import Web.DOM.HTMLCollection as Web.DOM.HTMLCollection
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument

type Element = Web.DOM.Element.Element

fromQueryMatch :: String -> Web.DOM.ParentNode.ParentNode -> Effect.Effect (Maybe.Maybe Element)
fromQueryMatch query parentNode = do
  let querySelector = Web.DOM.ParentNode.QuerySelector query
  Web.DOM.ParentNode.querySelector querySelector parentNode

fromQueryMatches :: String -> Web.DOM.ParentNode.ParentNode -> Effect.Effect (Array Element)
fromQueryMatches query parentNode = do
  let querySelector = Web.DOM.ParentNode.QuerySelector query
  Web.DOM.ParentNode.querySelectorAll querySelector parentNode >>= fromNodeList

fromNodeList :: Web.DOM.NodeList.NodeList -> Effect.Effect (Array Element)
fromNodeList nodeList = do
  Array.mapMaybe Web.DOM.Element.fromNode <$> Web.DOM.NodeList.toArray nodeList

removeChildren :: Element -> Effect.Effect Unit
removeChildren element = do
  let parent = Web.DOM.Element.toParentNode element
  children <- Web.DOM.ParentNode.children parent >>= \collection -> do
    elements <- Web.DOM.HTMLCollection.toArray collection
    pure $ Web.DOM.Element.toChildNode <$> elements
  Traversable.for_ children Web.DOM.ChildNode.remove

toIdentifier :: Effect.Ref.Ref Int -> Element -> Effect.Effect String
toIdentifier uuidRef element = do
  muuid <- MaybeT.runMaybeT do
    payload <- MaybeT.MaybeT $ Web.DOM.Element.getAttribute "data-uuid" element
    MaybeT.MaybeT $ pure $ Int.fromString payload
  uuid <- case muuid of
    Maybe.Just uuid -> do
      pure uuid
    _ -> do
      uuid <- Effect.Ref.modify ((+) 1) uuidRef
      Web.DOM.Element.setAttribute "data-uuid" (show uuid) element
      pure uuid
  pure $ show uuid

