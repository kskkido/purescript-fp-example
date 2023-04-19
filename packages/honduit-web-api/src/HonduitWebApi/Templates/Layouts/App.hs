module HonduitWebApi.Templates.Layouts.App
  ( render
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified HonduitWebApi.Templates.Components.AppHeader as AppHeader

render :: (Monad m) => [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) () -> Lucid.HtmlT (Reader.ReaderT a m) ()
render _ html = do
  Lucid.html_ do
    Lucid.head_ do
      Lucid.link_
        [ Lucid.rel_ "stylesheet"
        , Lucid.href_ $ Text.pack "/web/assets/styles/index.css"
        ]
      Lucid.script_
        [ Lucid.src_ $ Text.pack "/web/assets/scripts/index.js"
        , Lucid.defer_ ""
        ]
        ( Text.pack ""
        )
    Lucid.body_ [] do
      AppHeader.render
        [ Lucid.classes_ ["px-8", "py-6"]
        ]
      Lucid.main_
        [ Lucid.id_ "main"
        , Lucid.classes_ ["flex", "flex-col", "px-8", "sm:px-14", "mb-16", "sm:mb-32"]
        ]
        do
          Lucid.div_ [Lucid.id_ "page"] do
            html
