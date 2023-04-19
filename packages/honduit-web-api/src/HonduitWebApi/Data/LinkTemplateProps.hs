module HonduitWebApi.Data.LinkTemplateProps
  ( LinkTemplateProps(..)
  ) where

import RIO

data LinkTemplateProps = LinkTemplateProps
  { path :: String
  }
  deriving (Eq, Show)
