module HonduitWebApi.Templates.Pages.Home
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader

render :: (Monad m) => Lucid.HtmlT (Reader.ReaderT a m) ()
render = do
  Lucid.div_ [] do
    Lucid.toHtml ("home" :: String)
