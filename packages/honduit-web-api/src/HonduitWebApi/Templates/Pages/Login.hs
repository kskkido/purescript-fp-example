module HonduitWebApi.Templates.Pages.Login
  ( render
  ) where

import RIO
import qualified Lucid
import qualified Control.Monad.Reader as Reader

render :: (Monad m) => Lucid.HtmlT (Reader.ReaderT a m) ()
render = do
  Lucid.div_
    [ Lucid.id_ "login"
    , Lucid.classes_
      [ "flex"
      , "flex-col"
      , "justify-center"
      , "items-center"
      , "w-full"
      ]
    ]
    do
      Lucid.form_
        [ Lucid.id_ "login_form"
        , Lucid.classes_
          [ "flex"
          , "flex-col"
          , "items-center"
          , "bg-white"
          , "shadow-md"
          , "rounded"
          , "px-8"
          , "pt-6"
          , "pb-8"
          , "w-80"
          , "max-w-[90%]"
          ]
        ]
        do
          Lucid.div_
            [ Lucid.classes_
              [ "w-full"
              , "mb-4"
              ]
            ]
            do
              Lucid.label_
                [ Lucid.for_ "email"
                , Lucid.classes_
                  [ "block"
                  , "text-gray-700"
                  , "text-sm"
                  , "font-bold"
                  , "mb-2"
                  ]
                ]
                do
                  Lucid.toHtml ("email" :: String)
              Lucid.input_
                [ Lucid.id_ "email"
                , Lucid.name_ "email"
                , Lucid.type_ "email"
                , Lucid.classes_
                  [ "shadow"
                  , "appearance-none"
                  , "border"
                  , "rounded"
                  , "w-full"
                  , "py-2"
                  , "px-3"
                  , "text-gray-700"
                  , "leading-tight"
                  , "focus:outline-none"
                  , "focus:shadow-outline"
                  ]
                ]
          Lucid.div_
            [ Lucid.classes_
              [ "w-full"
              , "mb-6"
              ]
            ]
            do
              Lucid.label_
                [ Lucid.for_ "password"
                , Lucid.classes_
                  [ "block"
                  , "text-gray-700"
                  , "text-sm"
                  , "font-bold"
                  , "mb-2"
                  ]
                ]
                do
                  Lucid.toHtml ("password" :: String)
              Lucid.input_
                [ Lucid.id_ "password"
                , Lucid.name_ "password"
                , Lucid.type_ "password"
                , Lucid.classes_
                  [ "shadow"
                  , "appearance-none"
                  , "border"
                  , "rounded"
                  , "w-full"
                  , "py-2"
                  , "px-3"
                  , "text-gray-700"
                  , "mb-3"
                  , "leading-tight"
                  , "focus:outline-none"
                  , "focus:shadow-outline"
                  ]
                ]
          Lucid.div_
            [ Lucid.classes_
              [ "w-full"
              ]
            ]
            do
              Lucid.input_
                [ Lucid.type_ "submit"
                , Lucid.value_ "submit"
                , Lucid.classes_
                  [ "bg-blue-500"
                  , "hover:bg-blue-700"
                  , "text-white"
                  , "font-bold"
                  , "py-2"
                  , "px-4"
                  , "rounded"
                  , "focus:outline-none"
                  , "focus:shadow-outline"
                  ]
                ]
