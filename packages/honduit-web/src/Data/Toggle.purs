module Data.Toggle where

data Toggle =
    Ping
  | Pong

toggle :: Toggle -> Toggle
toggle Ping = Pong
toggle Pong = Ping
