module HonduitWebApi.Templates.Components.ArticleFeedItem
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified HonduitWebApi.Lib.Lucid.Data.Attribute as Attribute
import qualified HonduitWebApi.Data.AuthUserArticle as AuthUserArticle
import qualified HonduitWebApi.Data.Tag as Tag

render :: (Monad m) => AuthUserArticle.AuthUserArticle -> [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
render article attributes = do
  Lucid.div_
    ( [ Lucid.classes_
        [
        ]
      ] `Attribute.concat`
      attributes
    )
    do
      Lucid.div_
        [
        ]
        do
          Lucid.a_
            [
            ]
            do
              Lucid.img_
                [ Lucid.src_ ""
                ]
          Lucid.div_
            [
            ]
            do
              Lucid.a_
                [
                ]
                do
                  Lucid.toHtml ("foobar" :: String)
              Lucid.span_
                [
                ]
                do
                  Lucid.toHtml $ show article.createdAt
          Lucid.div_
            [
            ]
            do
              Lucid.toHtml $ show article.favoritesCount
      Lucid.a_
        [
        ]
        do
          Lucid.h1_
            [
            ]
            do
              Lucid.toHtml article.title
          Lucid.p_
            [
            ]
            do
              Lucid.toHtml article.description
          Lucid.div_
            [
            ]
            do
              Lucid.span_
                [
                ]
                do
                  Lucid.toHtml ("Read more..." :: String)
              Lucid.ul_
                [
                ]
                do
                  flip foldMap article.tags $ \(Tag.Tag _ label) -> do
                    Lucid.li_
                      [
                      ]
                      do
                        Lucid.toHtml label
