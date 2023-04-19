module HonduitWebApi.Services.AppApiServer
  ( toApp
  ) where

import RIO
import qualified System.IO as IO
import qualified Servant
import qualified Servant.API
import qualified Lucid
import qualified Control.Monad.Catch as Catch
import qualified Control.Monad.Reader as Reader
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified HonduitWebApi.Lib.Jwt as Jwt
import qualified HonduitWebApi.Data.AuthUser as AuthUser
import qualified HonduitWebApi.Data.AppConfig as AppConfig
import qualified HonduitWebApi.Data.AppContext as AppContext
import qualified HonduitWebApi.Data.AuthHeaders as AuthHeaders
import qualified HonduitWebApi.Data.AuthConfig as AuthConfig
import qualified HonduitWebApi.Data.ArticleFeedFilter as ArticleFeedFilter
import qualified HonduitWebApi.Data.AuthUserArticleRepositoryInterface as AuthUserArticleRepositoryInterface
import qualified HonduitWebApi.Data.User as User
import qualified HonduitWebApi.Data.UserRepositoryInterface as UserRepositoryInterface
import qualified HonduitWebApi.Data.UserLoginApiRequestBody as UserLoginApiRequestBody
import qualified HonduitWebApi.Data.UserLoginApiResponse as UserLoginApiResponse
import qualified HonduitWebApi.Data.UserLoginCredential as UserLoginCredential
import qualified HonduitWebApi.Data.AuthUserArticleFeedFilter as AuthUserArticleFeedFilter
import qualified HonduitWebApi.Data.AuthUserArticleFeedFilterPayload as AuthUserArticleFeedFilterPayload
import qualified HonduitWebApi.Data.ArticleFeedTemplateProps as ArticleFeedTemplateProps
import qualified HonduitWebApi.Templates.Components.ArticleFeed as Templates.Components.ArticleFeed
import qualified HonduitWebApi.Templates.Pages.Home as Templates.Pages.Home
import qualified HonduitWebApi.Templates.Pages.Posts as Templates.Pages.Posts
import qualified HonduitWebApi.Templates.Pages.Login as Templates.Pages.Login
import qualified HonduitWebApi.Templates.Pages.Register as Templates.Pages.Register
import qualified HonduitWebApi.Templates.Pages.NotFound as Templates.Pages.NotFound
import qualified HonduitWebApi.Templates.Layouts.App as Templates.Layouts.App
import qualified HonduitWebApi.Services.AppApi as AppApi

type AppApiServer m = Reader.ReaderT AppContext.AppContext m
type AppApiServerHandler = AppApiServer Servant.Handler

toApp :: Monad m => AppApiServer m Servant.Application
toApp = do
  context <- Reader.ask
  pure $
    Servant.serveWithContextT
      AppApi.appApiProxy
      ( handleAuthOptionalRoute context Servant.:.
        Servant.EmptyContext
      )
      ( `Reader.runReaderT` context )
      ( ( handleApiUserLogin
        ) Servant.:<|>
        ( ( handleWebHome
          ) Servant.:<|>
          ( ( handleWebPosts
            ) Servant.:<|>
            ( handleWebPostsFeed
            )
          ) Servant.:<|>
          ( handleWebLogin
          ) Servant.:<|>
          ( handleWebRegister
          ) Servant.:<|>
          ( handleWeb404
          ) Servant.:<|>
          ( Servant.serveDirectoryWebApp context.appConfig.publicAssetsFilePath
          )
        ) Servant.:<|>
        ( handleWebApp
        )
      )

handleAuthOptionalRoute :: AppContext.AppContext ->  Servant.BasicAuthCheck (Maybe AuthUser.AuthUser)
handleAuthOptionalRoute context = Servant.BasicAuthCheck \basicAuthData -> do
  authUser <- liftIO do
    let credential = UserLoginCredential.fromBasicAuthData basicAuthData
    user <- context.userRepository.getUserByLoginCredential credential
    pure $ Just $ AuthUser.AuthUser user.id
    `Catch.catchAll` \_ -> do
      pure Nothing
  pure $ Servant.Authorized authUser

handleApiUserLogin :: UserLoginApiRequestBody.UserLoginApiRequestBody -> AppApiServerHandler ( Servant.API.Headers AuthHeaders.AuthHeaders UserLoginApiResponse.UserLoginApiResponse )
handleApiUserLogin requestBody = do
  context <- Reader.ask
  muser <- liftIO do
    let credential = UserLoginApiRequestBody.toUserLoginCredential requestBody
    context.userRepository.findUserByLoginCredential credential
  case muser of
    Nothing -> do
      Servant.throwError $ Servant.err401 { Servant.errBody = "mismatchingLoginCredential" }
    Just user -> do
      let response = UserLoginApiResponse.fromUser user
      cookie <- liftIO $ AuthConfig.toCookie user context
      pure $ Servant.API.addHeader cookie response

handleWebHome :: Maybe AuthUser.AuthUser -> AppApiServerHandler ByteString.Lazy.ByteString
handleWebHome _ = do
  liftIO $ IO.putStrLn $ "Handling web home"
  Lucid.renderBST Templates.Pages.Home.render

handleWebPosts :: Maybe AuthUser.AuthUser -> AppApiServerHandler ByteString.Lazy.ByteString
handleWebPosts _ = do
  liftIO $ IO.putStrLn $ "Handling web posts"
  Lucid.renderBST Templates.Pages.Posts.render

handleWebPostsFeed :: Maybe AuthUser.AuthUser -> Maybe String -> Maybe Int -> Maybe Bool -> Maybe Int -> Maybe Int -> AppApiServerHandler ByteString.Lazy.ByteString
handleWebPostsFeed muser = AuthUserArticleFeedFilterPayload.fromQuery \payload -> do
  feedFilter <- AuthUserArticleFeedFilter.fromPayload payload
  articles <- do
    context <- Reader.ask
    liftIO do
      case muser of
        Just user -> do
          context.authUserArticleRepository.getFeedByFilterByUser feedFilter user
        _ -> do
          context.authUserArticleRepository.getFeedByFilter feedFilter
  Lucid.renderBST $
    Templates.Components.ArticleFeed.render
      ( ArticleFeedTemplateProps.ArticleFeedTemplateProps
          feedFilter
          articles
      )
      []

handleWebLogin :: Maybe AuthUser.AuthUser -> AppApiServerHandler ByteString.Lazy.ByteString
handleWebLogin _ = do
  liftIO $ IO.putStrLn $ "Handling web login"
  Lucid.renderBST Templates.Pages.Login.render

handleWebRegister :: Maybe AuthUser.AuthUser -> AppApiServerHandler ByteString.Lazy.ByteString
handleWebRegister _ = do
  liftIO $ IO.putStrLn $ "Handling web register"
  Lucid.renderBST Templates.Pages.Register.render

handleWeb404 :: Maybe AuthUser.AuthUser -> AppApiServerHandler ByteString.Lazy.ByteString
handleWeb404 _ = do
  liftIO $ IO.putStrLn $ "Handling web 404"
  Lucid.renderBST Templates.Pages.NotFound.render

handleWebApp :: Maybe AuthUser.AuthUser -> [String] -> AppApiServerHandler ByteString.Lazy.ByteString
handleWebApp _ segments = do
  liftIO $ IO.putStrLn $ "Handling web app" <> show segments
  Lucid.renderBST (Templates.Layouts.App.render [] mempty)
