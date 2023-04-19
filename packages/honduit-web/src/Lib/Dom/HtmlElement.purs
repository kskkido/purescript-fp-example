module Lib.Dom.HtmlElement where

import Prelude
import Effect as Effect
import Effect.Ref as Effect.Ref
import Foreign as Foreign
import Foreign.Object as Foreign.Object
import Control.MonadZero as Control.MonadZero
import Control.Monad.Trans.Class as Trans
import Control.Monad.Maybe.Trans as MaybeT
import Data.Array as Array
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Number as Number
import Data.Either as Either
import Data.Foldable as Foldable
import Data.Traversable as Traversable
import Data.Map as Map
import Type.Row.Homogeneous as Homogeneous
import Web.DOM.Node as Web.DOM.Node
import Web.DOM.Element as Web.DOM.Element
import Web.DOM.Node as Web.DOM.Node
import Web.DOM.NodeList as Web.DOM.NodeList
import Web.DOM.ChildNode as Web.DOM.ChildNode
import Web.DOM.ParentNode as Web.DOM.ParentNode
import Web.DOM.DOMTokenList as Web.DOM.DOMTokenList
import Web.DOM.HTMLCollection as Web.DOM.HTMLCollection
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Lib.Dom.Element as Element

type HtmlElement = Web.HTML.HTMLElement.HTMLElement

foreign import fromStringImpl :: (HtmlElement -> Maybe.Maybe HtmlElement) -> (Maybe.Maybe HtmlElement) -> String -> Effect.Effect (Maybe.Maybe HtmlElement)
foreign import setStylesImpl :: Foreign.Object.Object String -> HtmlElement -> Effect.Effect Unit

fromString :: String -> Effect.Effect (Maybe.Maybe HtmlElement)
fromString = fromStringImpl Maybe.Just Maybe.Nothing

fromQueryMatch :: String -> Web.DOM.ParentNode.ParentNode -> Effect.Effect (Maybe.Maybe HtmlElement)
fromQueryMatch query parentNode = do
  melement <- Element.fromQueryMatch query parentNode
  pure $ (melement >>= Web.HTML.HTMLElement.fromElement)

fromQueryMatches :: String -> Web.DOM.ParentNode.ParentNode -> Effect.Effect (Array HtmlElement)
fromQueryMatches query parentNode = do
  elements <- Element.fromQueryMatches query parentNode
  pure $ Array.mapMaybe Web.HTML.HTMLElement.fromElement elements

removeChildren :: HtmlElement -> Effect.Effect Unit
removeChildren htmlElement = do
  let parent = Web.HTML.HTMLElement.toParentNode htmlElement
  children <- Web.DOM.ParentNode.children parent >>= \collection -> do
    elements <- Web.DOM.HTMLCollection.toArray collection
    pure $ Web.DOM.Element.toChildNode <$> elements
  Traversable.for_ children Web.DOM.ChildNode.remove

appendHtmlElement :: HtmlElement -> HtmlElement -> Effect.Effect Unit
appendHtmlElement from to = do
  let child = Web.HTML.HTMLElement.toNode from
      parent = Web.HTML.HTMLElement.toNode to
  Web.DOM.Node.appendChild child parent

toIdentifier :: Effect.Ref.Ref Int -> HtmlElement -> Effect.Effect String
toIdentifier ref htmlElement = do
  Element.toIdentifier ref (Web.HTML.HTMLElement.toElement htmlElement)

setStyles :: forall r. Homogeneous.Homogeneous r String => { | r } -> HtmlElement -> Effect.Effect Unit
setStyles = setStylesImpl <<< Foreign.Object.fromHomogeneous
