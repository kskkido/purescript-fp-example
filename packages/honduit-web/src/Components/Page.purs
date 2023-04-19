module Components.Page where

import Prelude
import Control.Monad.Reader as Reader
import Web.HTML.HTMLElement as HTMLElement
import Data.Animation as Animation
import Data.DomAnimation as DomAnimation

animate :: HTMLElement.HTMLElement -> Animation.Animation
animate element = do
  Animation.merge
    [ DomAnimation.slideInBottom element
    , DomAnimation.fadeIn element
    ]

