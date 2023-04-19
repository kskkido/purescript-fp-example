module HonduitWebApi.Lib.Jwt
  ( getPayload
  , setPayload
  ) where

import RIO
import qualified RIO.Text as Text
import qualified Crypto.JWT as JWT
import qualified Data.Map as Map
import qualified Data.Aeson as Aeson
import qualified Data.Foldable as Foldable

getPayload :: Aeson.FromJSON a => String -> JWT.ClaimsSet -> Maybe a
getPayload key claimsSet = do
  value <- Map.lookup (Text.pack key) (claimsSet ^. JWT.unregisteredClaims)
  Foldable.asum $ Aeson.fromJSON value

setPayload :: Aeson.ToJSON a => String -> a -> JWT.ClaimsSet -> JWT.ClaimsSet
setPayload key a = JWT.addClaim (Text.pack key) (Aeson.toJSON a)

