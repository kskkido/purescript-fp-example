module Lib.RemoteData where

import Network.RemoteData as RemoteData

equalStatus :: forall a b. RemoteData.RemoteData a b -> RemoteData.RemoteData a b -> Boolean
equalStatus RemoteData.NotAsked RemoteData.NotAsked = true
equalStatus RemoteData.Loading RemoteData.Loading = true
equalStatus (RemoteData.Failure _) (RemoteData.Failure _) = true
equalStatus (RemoteData.Success _) (RemoteData.Success _) = true
equalStatus _ _ = false

showStatus :: forall a b. RemoteData.RemoteData a b -> String
showStatus RemoteData.NotAsked = "NotAsked"
showStatus RemoteData.Loading = "Loading"
showStatus (RemoteData.Failure _) = "Failure"
showStatus (RemoteData.Success _) = "Success"
