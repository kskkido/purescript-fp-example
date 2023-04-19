module Data.RenderPhase where

import Prelude
import Data.Generic.Rep as Generic

data RenderPhase =
    Preload
  | Intro
  | Main
  | Outro

derive instance renderPhaseActionGeneric :: Generic.Generic RenderPhase _
instance renderPhaseActionShow :: Show RenderPhase where
  show = case _ of
    Preload -> "preload"
    Intro -> "intro"
    Main -> "main"
    Outro -> "outro"

