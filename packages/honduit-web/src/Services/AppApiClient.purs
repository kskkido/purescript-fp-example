module Services.AppApiClient where

import Prelude
import Effect.Aff as Effect.Aff
import Effect.Class as Effect.Class
import Affjax as Affjax
import Affjax.ResponseFormat as Affjax.ResponseFormat
import Network.RemoteData as RemoteData
import Control.Monad.Trans.Class as Trans
import Control.Monad.Reader as Reader
import Lib.Dom.HtmlElement as HtmlElement
import Data.Either as Either
import Data.Maybe as Maybe
import Data.AppApiClientContext as AppApiClientContext
import Data.AppApiClientInterface as AppApiClientInterface

type AppApiClient = Reader.ReaderT AppApiClientContext.AppApiClientContext

toInterface :: AppApiClientContext.AppApiClientContext -> AppApiClientInterface.AppApiClientInterface
toInterface context =
  { getHome: Reader.runReaderT getHome context
  , getPosts: Reader.runReaderT getPosts context
  , getLogin: Reader.runReaderT getLogin context
  , getRegister: Reader.runReaderT getRegister context
  , get404: Reader.runReaderT get404 context
  }

getHome :: AppApiClient Effect.Aff.Aff (RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement)
getHome = do
  result <- Trans.lift do
    Affjax.get Affjax.ResponseFormat.string "/web/home"
  RemoteData.fromEither <$> case result of
    Either.Right payload -> do
      parseHtml payload
    Either.Left error -> do
      pure (Either.Left error)

getPosts :: AppApiClient Effect.Aff.Aff (RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement)
getPosts = do
  result <- Trans.lift do
    Affjax.get Affjax.ResponseFormat.string "/web/posts"
  RemoteData.fromEither <$> case result of
    Either.Right payload -> do
      parseHtml payload
    Either.Left error -> do
      pure (Either.Left error)

getLogin :: AppApiClient Effect.Aff.Aff (RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement)
getLogin = do
  result <- Trans.lift do
    Affjax.get Affjax.ResponseFormat.string "/web/login"
  RemoteData.fromEither <$> case result of
    Either.Right payload -> do
      parseHtml payload
    Either.Left error -> do
      pure (Either.Left error)

getRegister :: AppApiClient Effect.Aff.Aff (RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement)
getRegister = do
  result <- Trans.lift do
    Affjax.get Affjax.ResponseFormat.string "/web/register"
  RemoteData.fromEither <$> case result of
    Either.Right payload -> do
      parseHtml payload
    Either.Left error -> do
      pure (Either.Left error)

get404 :: AppApiClient Effect.Aff.Aff (RemoteData.RemoteData Affjax.Error HtmlElement.HtmlElement)
get404 = do
  result <- Trans.lift do
    Affjax.get Affjax.ResponseFormat.string "/web/404"
  RemoteData.fromEither <$> case result of
    Either.Right payload -> do
      parseHtml payload
    Either.Left error -> do
      pure (Either.Left error)

parseHtml :: forall m. Effect.Class.MonadEffect m => Affjax.Response String -> AppApiClient m (Either.Either Affjax.Error HtmlElement.HtmlElement)
parseHtml response = do
  context <- Reader.ask
  melement <- Trans.lift $ Effect.Class.liftEffect $ do
    HtmlElement.fromString response.body
  pure $ case melement of
    Maybe.Just element -> Either.Right element
    Maybe.Nothing -> Either.Left (Affjax.RequestContentError "Expected html string")
