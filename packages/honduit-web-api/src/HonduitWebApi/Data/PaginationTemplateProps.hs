module HonduitWebApi.Data.PaginationTemplateProps
  ( PaginationTemplateProps(..)
  ) where

import RIO
import qualified HonduitWebApi.Data.PaginationMetadata as PaginationMetadata

data PaginationTemplateProps = PaginationTemplateProps
  { metadata :: PaginationMetadata.PaginationMetadata
  , length :: Int
  , lookaround :: Int
  , onClick :: PaginationMetadata.PaginationMetadata -> String
  }

