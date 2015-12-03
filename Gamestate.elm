module Gamestate where

stop gs =
    { gs | stopped = True }
