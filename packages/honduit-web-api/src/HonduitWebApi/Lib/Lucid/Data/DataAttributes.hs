module HonduitWebApi.Lib.Lucid.Data.DataAttributes
  ( DataAttributes
  ) where

import qualified RIO.Map as Map
import qualified RIO.Text as Text

type DataAttributes = Map.Map Text.Text Text.Text

