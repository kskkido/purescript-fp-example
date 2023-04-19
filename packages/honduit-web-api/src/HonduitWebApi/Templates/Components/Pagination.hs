module HonduitWebApi.Templates.Components.Pagination
  ( render
  ) where

import RIO
import qualified RIO.List as List
import qualified Lucid
import qualified Lucid.Svg
import qualified Control.Monad.Reader as Reader
import qualified HonduitWebApi.Lib.Lucid.Data.Attribute as Attribute
import qualified HonduitWebApi.Data.PaginationMetadata as PaginationMetadata
import qualified HonduitWebApi.Data.PaginationTemplateProps as PaginationTemplateProps
import qualified HonduitWebApi.Data.LinkTemplateProps as LinkTemplateProps
import qualified HonduitWebApi.Templates.Components.Link as Components.Link

render :: (Monad m) => PaginationTemplateProps.PaginationTemplateProps -> [Lucid.Attribute] -> Lucid.HtmlT (Reader.ReaderT a m) ()
render props attributes = do
  Lucid.nav_
    ( [ Lucid.classes_
        [ "isolate"
        , "inline-flex"
        , "-space-x-px"
        , "rounded-md"
        , "shadow-sm"
        ]
      ] `Attribute.concat` attributes
    )
    do
      do
        let next = props.metadata { PaginationMetadata.page = props.metadata.page - 1 }
        Components.Link.render
          ( LinkTemplateProps.LinkTemplateProps
            ( props.onClick next )
          )
          [ Lucid.href_ "#"
          , Lucid.classes_
            [ "relative"
            , "inline-flex"
            , "items-center"
            , "rounded-l-md"
            , "px-2"
            , "py-2"
            , "text-gray-400"
            , "ring-1"
            , "ring-inset"
            , "ring-gray-300"
            , "hover:bg-gray-50"
            , "focus:z-20"
            , "focus:outline-offset-0"
            ]
          ]
          do
            Lucid.Svg.svg_
              [ Lucid.Svg.viewBox_ "0 0 20 20"
              , Lucid.Svg.fill_ "currentColor"
              , Lucid.classes_
                [ "h-5"
                , "w-5"
                ]
              ]
              do
                Lucid.Svg.path_
                  [ Lucid.Svg.fill_rule_ "evenodd"
                  , Lucid.Svg.clip_rule_ "evenodd"
                  , Lucid.Svg.d_ "M12.79 5.23a.75.75 0 01-.02 1.06L8.832 10l3.938 3.71a.75.75 0 11-1.04 1.08l-4.5-4.25a.75.75 0 010-1.08l4.5-4.25a.75.75 0 011.06.02z"
                  ]
      fold $ List.intercalate
        [ Lucid.span_
            [ Lucid.classes_
              [ "relative"
              , "inline-flex"
              , "items-center"
              , "px-4"
              , "py-2"
              , "text-sm"
              , "font-semibold"
              , "text-gray-700"
              , "ring-1"
              , "ring-inset"
              , "ring-gray-300"
              , "focus:outline-offset-0"
              ]
            ]
            do
              Lucid.toHtml ("..." :: String)
        ]
        ( ( PaginationMetadata.toPagination props.length props.lookaround props.metadata ) <&>
          ( fmap $ \slot -> do
              let next = props.metadata { PaginationMetadata.page = slot }
              Components.Link.render
                ( LinkTemplateProps.LinkTemplateProps
                  ( props.onClick next )
                )
                [ Lucid.href_ "#"
                , Lucid.classes_
                  [ "relative"
                  , "inline-flex"
                  , "items-center"
                  , "px-4"
                  , "py-2"
                  , "text-sm"
                  , "font-semibold"
                  , "text-gray-900"
                  , "ring-1"
                  , "ring-inset"
                  , "ring-gray-300"
                  , "hover:bg-gray-50"
                  , "focus:z-20"
                  , "focus:outline-offset-0"
                  ]
                ]
                do
                  Lucid.toHtml (show slot)
          )
        )
      do
        let next = props.metadata { PaginationMetadata.page = props.metadata.page + 1 }
        Components.Link.render
          ( LinkTemplateProps.LinkTemplateProps
            ( props.onClick next )
          )
          [ Lucid.href_ "#"
          , Lucid.classes_
            [ "relative"
            , "inline-flex"
            , "items-center"
            , "rounded-r-md"
            , "px-2"
            , "py-2"
            , "text-gray-400"
            , "ring-1"
            , "ring-inset"
            , "ring-gray-300"
            , "hover:bg-gray-50"
            , "focus:z-20"
            , "focus:outline-offset-0"
            ]
          ]
          do
            Lucid.Svg.svg_
              [ Lucid.Svg.viewBox_ "0 0 20 20"
              , Lucid.Svg.fill_ "currentColor"
              , Lucid.classes_
                [ "h-5"
                , "w-5"
                ]
              ]
              do
                Lucid.Svg.path_
                  [ Lucid.Svg.fill_rule_ "evenodd"
                  , Lucid.Svg.clip_rule_ "evenodd"
                  , Lucid.Svg.d_ "M7.21 14.77a.75.75 0 01.02-1.06L11.168 10 7.23 6.29a.75.75 0 111.04-1.08l4.5 4.25a.75.75 0 010 1.08l-4.5 4.25a.75.75 0 01-1.06-.02z"
                  ]
