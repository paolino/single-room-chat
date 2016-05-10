module Protocol where

import Data.Text

data Protocol = Login Text | Message Text | Logout deriving Read

data MessageFrom = MessageFrom Text Text deriving (Show,Read)

data ClientMessage = Regular MessageFrom | Problem Text deriving (Show,Read)
