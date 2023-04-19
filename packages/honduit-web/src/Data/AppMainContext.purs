module Data.AppMainContext where

import Prelude
import Effect as Effect
import Effect.Ref as Effect.Ref
import Effect.Class as Effect.Class
import Web.HTML.Window as Web.HTML.Window
import Web.HTML.Location as Web.HTML.Location
import Web.HTML.HTMLElement as Web.HTML.HTMLElement
import Web.HTML.HTMLDocument as Web.HTML.HTMLDocument
import Control.Monad.Reader as Reader
import Control.Monad.Maybe.Trans as MaybeT
import Data.AppApiClientInterface as AppApiClientInterface
import Data.AppLocation as AppLocation
import Data.HistoryStore as HistoryStore
import Data.RouteToken as RouteToken
import Data.List as List
import Data.Path as Path
import Data.PathParameters as PathParameters
import Data.PathValidator as PathValidator
import Data.Validator as Validator
import Lib.Plus as Plus
import Lib.Dom.HtmlElement as HtmlElement
import Lib.Observable as Observable
import Services.AppApiClient as AppApiClient

type AppMainContext =
  { window :: Web.HTML.Window.Window
  , uuid :: Effect.Ref.Ref Int
  , main :: Web.HTML.HTMLElement.HTMLElement
  , page :: Web.HTML.HTMLElement.HTMLElement
  , time :: Observable.Observable Number
  , historyStore :: HistoryStore.HistoryStore
  , appApiClient :: AppApiClientInterface.AppApiClientInterface
  , pathValidator :: String -> AppLocation.AppLocation
  }

fromWindow :: forall m. Effect.Class.MonadEffect m => Web.HTML.Window.Window -> MaybeT.MaybeT m AppMainContext
fromWindow window = do
  uuid <- Effect.Class.liftEffect do
    Effect.Ref.new 0
  document <- Effect.Class.liftEffect do
    Web.HTML.Window.document window
  historyStore <- Effect.Class.liftEffect do
    HistoryStore.fromWindow window
  time <- Effect.Class.liftEffect do
    Observable.singleton (Observable.fromAnimationLoop window)
  main <- MaybeT.MaybeT $ Effect.Class.liftEffect do
    let parent = Web.HTML.HTMLDocument.toParentNode document
    HtmlElement.fromQueryMatch "#main" parent
  page <- MaybeT.MaybeT $ Effect.Class.liftEffect do
    let parent = Web.HTML.HTMLDocument.toParentNode document
    HtmlElement.fromQueryMatch "#page" parent
  pure $
    { window: window
    , uuid: uuid
    , main: main
    , page: page
    , time
    , historyStore: historyStore
    , appApiClient: AppApiClient.toInterface {}
    , pathValidator:
        ( ( [ ( PathValidator.match
                  ( List.Nil
                  )
              ) $> AppLocation.Home
            , ( PathValidator.match
                  ( [ RouteToken.Literal "login"
                    ] # List.fromFoldable
                  )
              ) $> AppLocation.Login
            , ( PathValidator.match
                  ( [ RouteToken.Literal "register"
                    ] # List.fromFoldable
                  )
              ) $> AppLocation.Register
            , ( PathValidator.match
                  ( [ RouteToken.Literal "posts"
                    ] # List.fromFoldable
                  )
              ) $> AppLocation.Posts
            ]
          ) #
          ( Plus.sum <<< List.fromFoldable ) #
          ( Validator.fold
              ( const AppLocation.NotFound )
              ( identity )
          )
        ) <<< Path.fromString
    }

toPathname :: forall m. Effect.Class.MonadEffect m => AppMainContext -> m String
toPathname context = do
  Effect.Class.liftEffect do
    location <- Web.HTML.Window.location context.window
    Web.HTML.Location.pathname location
