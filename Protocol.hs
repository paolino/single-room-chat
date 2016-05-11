module Protocol where

import Data.Text

data Protocol = Login Text | Message Text | Logout deriving Read

data MessageCore 
    = MessageFrom Text Text  
    | Join Text
    | Leave Text
    | Renick Text Text
    deriving (Show,Read)


data ClientMessage = Regular MessageCore | Problem Text deriving (Show,Read)
