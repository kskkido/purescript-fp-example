module HonduitWebApi.Data.HtmlContentType
  ( HtmlContentType(..)
  ) where

import qualified Network.HTTP.Media as Media
import qualified Servant.API
import qualified Data.ByteString.Lazy as ByteString.Lazy

data HtmlContentType = HtmlContentType

instance Servant.API.Accept HtmlContentType where
  contentType _ = "text" Media.// "html" Media./: ("charset", "utf-8")
instance Servant.API.MimeRender HtmlContentType ByteString.Lazy.ByteString where
  mimeRender _ bs = bs
