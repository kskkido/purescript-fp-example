module HonduitWebApi.Services.AppApi
  ( AppApi
  , appApiProxy
  ) where

import RIO
import qualified Servant
import qualified Servant.API
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified HonduitWebApi.Data.AuthUserOptionalRoute as AuthUserOptionalRoute
import qualified HonduitWebApi.Data.AuthHeaders as AuthHeaders
import qualified HonduitWebApi.Data.HtmlContentType as HtmlContentType
import qualified HonduitWebApi.Data.UserLoginApiRequestBody as UserLoginApiRequestBody
import qualified HonduitWebApi.Data.UserLoginApiResponse as UserLoginApiResponse

type AppApi =
  ( "api" ) Servant.API.:>
  ( ( "users" ) Servant.API.:>
    ( ( "login" ) Servant.API.:>
      ( Servant.API.ReqBody '[Servant.API.JSON] UserLoginApiRequestBody.UserLoginApiRequestBody ) Servant.API.:>
      ( Servant.API.Post '[Servant.API.JSON] ( Servant.API.Headers AuthHeaders.AuthHeaders UserLoginApiResponse.UserLoginApiResponse ))
    )
  ) Servant.API.:<|>
  ( "web" ) Servant.API.:>
  ( ( "home" ) Servant.API.:>
    ( AuthUserOptionalRoute.AuthUserOptionalRoute ) Servant.API.:>
    ( Servant.API.Get '[HtmlContentType.HtmlContentType] ByteString.Lazy.ByteString ) Servant.API.:<|>
    ( "posts" ) Servant.API.:>
    ( ( AuthUserOptionalRoute.AuthUserOptionalRoute ) Servant.API.:>
      ( Servant.API.Get '[HtmlContentType.HtmlContentType] ByteString.Lazy.ByteString ) Servant.API.:<|>
      ( "feed" ) Servant.API.:>
      ( AuthUserOptionalRoute.AuthUserOptionalRoute ) Servant.API.:>
      ( Servant.API.QueryParam "tag" String ) Servant.API.:>
      ( Servant.API.QueryParam "authorId" Int ) Servant.API.:>
      ( Servant.API.QueryParam "favorited" Bool ) Servant.API.:>
      ( Servant.API.QueryParam "limit" Int ) Servant.API.:>
      ( Servant.API.QueryParam "offset" Int ) Servant.API.:>
      ( Servant.API.Get '[HtmlContentType.HtmlContentType] ByteString.Lazy.ByteString )
    ) Servant.API.:<|>
    ( "login" ) Servant.API.:>
    ( AuthUserOptionalRoute.AuthUserOptionalRoute ) Servant.API.:>
    ( Servant.API.Get '[HtmlContentType.HtmlContentType] ByteString.Lazy.ByteString ) Servant.API.:<|>
    ( "register" ) Servant.API.:>
    ( AuthUserOptionalRoute.AuthUserOptionalRoute ) Servant.API.:>
    ( Servant.API.Get '[HtmlContentType.HtmlContentType] ByteString.Lazy.ByteString ) Servant.API.:<|>
    ( "404" ) Servant.API.:>
    ( AuthUserOptionalRoute.AuthUserOptionalRoute ) Servant.API.:>
    ( Servant.API.Get '[HtmlContentType.HtmlContentType] ByteString.Lazy.ByteString ) Servant.API.:<|>
    ( "assets" ) Servant.API.:>
    ( Servant.API.Raw )
  ) Servant.API.:<|>
  ( AuthUserOptionalRoute.AuthUserOptionalRoute ) Servant.API.:>
  ( ( Servant.API.CaptureAll "segments" String ) Servant.API.:>
    ( Servant.API.Get '[HtmlContentType.HtmlContentType] ByteString.Lazy.ByteString )
  )

appApiProxy :: Servant.Proxy AppApi
appApiProxy = Servant.Proxy
