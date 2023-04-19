module HonduitWebApi.Templates.Components.Link
  ( render
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified HonduitWebApi.Lib.Lucid.Data.Attribute as Attribute
import qualified HonduitWebApi.Data.LinkTemplateProps as LinkTemplateProps

render :: (Monad m) => LinkTemplateProps.LinkTemplateProps -> [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) () ->Lucid.HtmlT (Reader.ReaderT a m) ()
render props attributes html = do
  Lucid.a_
    ( [ Lucid.data_ "component-type" "link"
      , Lucid.data_ "path" (Text.pack props.path)
      ] `Attribute.concat`
      attributes
    )
    do
      html
