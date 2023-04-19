module Services.App where

import Prelude
import Effect as Effect
import Control.Monad.Reader as Reader
import Network.RemoteData as RemoteData
import Data.Map as Map
import Data.AppLocation as AppLocation
import Data.AppMainContext as AppMainContext
import Controllers.App as Controllers.App

type App = Reader.ReaderT AppMainContext.AppMainContext

main :: App Effect.Effect Unit
main = do
  context <- Reader.ask
  pathname <- AppMainContext.toPathname context
  Controllers.App.main $
    { location: context.pathValidator pathname
    , animationMap: Map.empty
    , pages:
      { home:
        { request: RemoteData.NotAsked
        }
      , posts:
        { request: RemoteData.NotAsked
        }
      , login:
        { request: RemoteData.NotAsked
        , form:
          { email: ""
          , password: ""
          }
        }
      , register:
        { request: RemoteData.NotAsked
        , form:
          { username: ""
          , email: ""
          , password: ""
          }
        }
      }
    }
