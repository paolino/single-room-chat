module Protocol where

import Data.Text

data Protocol = Rename | Message Text deriving Read

data MessageCore 
    = MessageFrom Text Text  
    | Join Text
    | Leave Text
    | Renick Text Text
    | Listen Text
    | Unlisten Text
    | Room Text
    
    deriving (Show,Read)


data ClientMessage = Regular MessageCore | Problem Text deriving (Show,Read)
