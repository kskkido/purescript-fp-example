module HonduitWebApi.Templates.Components.ArticleFeed
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader
import qualified HonduitWebApi.Lib.Lucid.Data.Attribute as Attribute
import qualified HonduitWebApi.Templates.Components.ArticleFeedItem as ArticleFeedItem
import qualified HonduitWebApi.Data.PaginatedResponse as PaginatedResponse
import qualified HonduitWebApi.Data.PaginationMetadata as PaginationMetadata
import qualified HonduitWebApi.Data.PaginationTemplateProps as PaginationTemplateProps
import qualified HonduitWebApi.Data.AuthUserArticleFeedFilter as AuthUserArticleFeedFilter
import qualified HonduitWebApi.Data.ArticleFeedTemplateProps as ArticleFeedTemplateProps
import qualified HonduitWebApi.Templates.Components.Pagination as Components.Pagination

render :: (Monad m) => ArticleFeedTemplateProps.ArticleFeedTemplateProps -> [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
render (ArticleFeedTemplateProps.ArticleFeedTemplateProps feedFilter articles) attributes = do
  Lucid.div_
    [
    ]
    do
      Lucid.div_
        ( [ Lucid.classes_
            [
            ]
          ] `Attribute.concat`
          attributes
        )
        do
          flip foldMap articles.item $ \article -> do
            ArticleFeedItem.render article []
      Lucid.div_
        [
        ]
        do
          Components.Pagination.render
            ( PaginationTemplateProps.PaginationTemplateProps
                ( PaginationMetadata.fromPaginatedResponse articles )
                9
                3
                ( \metadata ->
                    ( ( AuthUserArticleFeedFilter.AuthUserArticleFeedFilter
                          feedFilter.tag
                          feedFilter.authorId
                          feedFilter.favorited
                          metadata.pageSize
                          ( metadata.page * metadata.pageSize )
                      ) &
                      ( AuthUserArticleFeedFilter.toQueryString ) &
                      ( (<>) "/web/posts/feed?" )
                    )
                )
            )
            []
