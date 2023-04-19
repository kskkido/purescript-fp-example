module HonduitWebApi.Templates.Components.AppHeader
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified HonduitWebApi.Lib.Lucid.Data.Attribute as Attribute
import qualified HonduitWebApi.Data.LinkTemplateProps as LinkTemplateProps
import qualified HonduitWebApi.Templates.Components.Link as Link

render :: (Monad m) => [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
render attributes = do
  Lucid.header_
    ( [ Lucid.classes_ ["w-full", "flex", "flex-col", "justify-center"]
      ] `Attribute.concat`
      attributes
    ) do
    Lucid.nav_ [Lucid.classes_ ["flex", "flex-row", "justify-between"]] do
      Lucid.ul_ [Lucid.classes_ ["flex", "flex-row", "grow-1"]] do
        Lucid.li_ [Lucid.classes_ ["pr-8", "py-4", "rounded-xl", "transition", "duration-200"]] do
          Lucid.div_ [Lucid.classes_ ["clip"]] do
            Link.render
              ( LinkTemplateProps.LinkTemplateProps
                  "/"
              )
              []
              do
                Lucid.toHtml ("home" :: String)
        Lucid.li_ [Lucid.classes_ ["px-8", "py-4", "rounded-xl", "transition", "duration-200"]] do
          Lucid.div_ [Lucid.classes_ ["clip"]] do
            Link.render
              ( LinkTemplateProps.LinkTemplateProps
                  "/posts"
              )
              []
              do
                Lucid.toHtml ("posts" :: String)
        Lucid.li_ [Lucid.classes_ ["px-8", "py-4", "rounded-xl", "transition", "duration-200"]] do
          Lucid.div_ [Lucid.classes_ ["clip"]] do
            Link.render
              ( LinkTemplateProps.LinkTemplateProps
                  "/login"
              )
              []
              do
                Lucid.toHtml ("login" :: String)
        Lucid.li_ [Lucid.classes_ ["px-8", "py-4", "rounded-xl", "transition", "duration-200"]] do
          Lucid.div_ [Lucid.classes_ ["clip"]] do
            Link.render
              ( LinkTemplateProps.LinkTemplateProps
                  "/register"
              )
              []
              do
                Lucid.toHtml ("register" :: String)
      Lucid.ul_ [Lucid.classes_ ["flex", "flex-row", "grow-1"]] do
        Lucid.li_ [] do
          Lucid.button_ [] do
            Lucid.div_ [] do
              Lucid.div_ []do
                Lucid.a_ [] do
                  Lucid.span_ [] do
                    Lucid.toHtml ("Dark" :: String)
                Lucid.a_ [] do
                  Lucid.span_ [] do
                    Lucid.toHtml ("Light" :: String)
