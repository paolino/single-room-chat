module Protocol where

import Data.Text (Text)

-- data going to the server
data Output 
    = Rename -- change my name 
    | Message (Maybe Text) Text  -- send a message to a room or to root
    | Listen Text -- receive messages from a room
    | Unlisten Text -- stop receiving messages from a room
    | NewRoom (Maybe Text) Text -- istantiate a new room with the given description
    deriving (Read, Show)

-- data going to the client
data Input
    = MessageFrom Text (Maybe Text) Text  -- a message from a user from a room
    | Join Text -- a user joined
    | Leave Text -- a user left
    | Renick Text Text -- a user changed his name
    | Room Text (Maybe Text) Text Text -- a user created a roomwith the given descriptoion
        
    deriving (Show,Read)


-- server can express problems
data ClientMessage  = Regular Input 
                    | Problem Text deriving (Show,Read)
