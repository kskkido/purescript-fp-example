module HonduitWebApi.Data.AuthConfig
  ( AuthConfig(..)
  , HasAuthConfig(..)
  , toCookie
  , toJwtValidationSettings
  ) where

import RIO
import Control.Lens
import qualified System.IO as IO
import qualified Control.Monad.Fail as MonadFail
import qualified Control.Monad.Except as Except
import qualified Web.Cookie as Cookie
import qualified Crypto.JWT as JWT
import qualified Crypto.JOSE.JWK as JWK
import qualified Crypto.JOSE.JWS as JWS
import qualified Crypto.JOSE.Compact as Compact
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.UTF8 as ByteString.UTF8
import qualified Data.Time.Clock as Time.Clock
import qualified Data.ByteString.Lazy as ByteString.Lazy
import qualified HonduitWebApi.Lib.Either as Either
import qualified HonduitWebApi.Lib.Jwt as Jwt

data AuthConfig = AuthConfig
  { cookieName :: String
  , cookieMaxAge :: Time.Clock.DiffTime
  , jwtSecret :: ByteString.Lazy.ByteString
  , jwtClientId :: String
  , jwtExpiresIn :: Time.Clock.NominalDiffTime
  }
  deriving (Eq, Show)

class HasAuthConfig a where
  get :: a -> AuthConfig
instance HasAuthConfig AuthConfig where
  get = id

toCookie :: (Aeson.ToJSON a, HasAuthConfig b) => a -> b -> IO.IO Cookie.SetCookie
toCookie payload = do
  config <- get
  pure do
    jwt <-
      ( toJwt payload config &
        Either.toMonadFail (const $ MonadFail.fail "")
      )
    pure $ Cookie.defaultSetCookie
      { Cookie.setCookieName = ByteString.UTF8.fromString config.cookieName
      , Cookie.setCookieMaxAge = pure config.cookieMaxAge
      , Cookie.setCookieSecure = True
      , Cookie.setCookieHttpOnly = True
      , Cookie.setCookieSameSite = pure Cookie.sameSiteStrict
      , Cookie.setCookieValue = ByteString.Lazy.toStrict jwt
      }

toJwt :: (Aeson.ToJSON a, HasAuthConfig b) => a -> b -> IO.IO (Either JWT.JWTError ByteString.Lazy.ByteString)
toJwt payload = do
  config <- get
  jwk <- toJwk
  pure $ Except.runExceptT do
    alg <- JWK.bestJWSAlg jwk
    claims <- liftIO $ toJwtClaims payload config
    jwt <- JWT.signClaims jwk (JWS.newJWSHeader ((), alg)) claims
    pure $ Compact.encodeCompact jwt

toJwk :: HasAuthConfig a => a -> JWK.JWK
toJwk = do
  config <- get
  pure $ JWK.fromOctets config.jwtSecret

toJwtValidationSettings :: HasAuthConfig a => a -> JWT.JWTValidationSettings
toJwtValidationSettings = do
  config <- get
  pure $ JWT.defaultJWTValidationSettings (== (fromString config.jwtClientId))

toJwtClaims :: (Aeson.ToJSON a, HasAuthConfig b) => a -> b -> IO.IO JWT.ClaimsSet
toJwtClaims payload = do
  config <- get
  pure do
    time <- Time.Clock.getCurrentTime
    pure $ JWT.emptyClaimsSet
      & JWT.claimAud ?~ JWT.Audience [fromString config.jwtClientId]
      & JWT.claimExp ?~ JWT.NumericDate (Time.Clock.addUTCTime config.jwtExpiresIn time)
      & JWT.claimIat ?~ JWT.NumericDate time
      & Jwt.setPayload "payload" payload

