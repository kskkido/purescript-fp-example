module Main
  ( main
  ) where

import RIO
import qualified System.IO as IO
import qualified Network.Wai.Handler.Warp as Wai
import qualified Control.Monad.Reader as Reader
import qualified HonduitWebApi.Lib.ExceptT as ExceptT
import qualified HonduitWebApi.Data.AppConfig as AppConfig
import qualified HonduitWebApi.Data.AppContext as AppContext
import qualified HonduitWebApi.Services.AppApiServer as AppApiServer

main :: IO.IO ()
main = do
  config <- ExceptT.liftFail AppConfig.fromSystem
  context <- AppContext.fromAppConfig config
  app <- Reader.runReaderT AppApiServer.toApp context
  IO.putStrLn $ "Running on: " <> show config.serverPort
  Wai.run config.serverPort app
