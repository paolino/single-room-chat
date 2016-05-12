{-# LANGUAGE RecursiveDo, ScopedTypeVariables,ViewPatterns, GADTs, TemplateHaskell,NoMonomorphismRestriction,OverloadedStrings,FlexibleContexts #-}
import Reflex.Dom
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Monoid
import Data.Maybe
import Protocol
import Data.FileEmbed
import Lib (ES,MS,DS,BS,domMorph,ifNotMorph,domMorph',ifMorph,mergeDSums,pick,Plug)
import Text.Read (readMaybe)
import Data.ByteString (ByteString)
import Data.Dependent.Map 
import Data.GADT.Compare.TH
import GHCJS.DOM.Element (focus)
import Control.Monad (void, forM)
import Control.Monad.Trans (liftIO)
import Control.Lens
import Data.List

performArg :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (liftIO . f) x)

focusNow t = getPostBuild >>= performArg (const $ focus (t ^. textInput_element))

singleWSSend :: Reflex t => Event t ByteString -> WebSocketConfig t -> WebSocketConfig t
singleWSSend m =  webSocketConfig_send .~ (return <$> m)

host = "ws://lambdasistemi.net:50100"
-- host = "ws://192.168.1.119:50000"

data Rooma = Rooma {
    _roomName :: T.Text,
    _roomDescription :: T.Text
    }
    deriving Eq

makeLenses ''Rooma
encodeMessage = encodeUtf8 . T.pack 

messageEl ( (Regular (Room u mr dx m))) = do
    elClass "span" "sender" $ text (T.unpack u)
    elClass "span" "room" $ text $ "opened"
    b <- elClass "span" "content" $ button (T.unpack dx)
    return $ Just (Rooma m dx <$ b)
messageEl ( (Regular (MessageFrom u mr m))) = do
    elClass "span" "sender" $ text (T.unpack u)
    elClass "span" "content" $ text (T.unpack m)
    return Nothing
messageEl ( (Regular (Join u))) = do
    elClass "span" "sender" $ text (T.unpack u)
    elClass "span" "action" $ text "joined"
    return Nothing
messageEl ( (Regular (Leave u))) = do
    elClass "span" "sender" $ text (T.unpack u)
    elClass "span" "action" $ text "leaved"
    return Nothing
messageEl ( (Regular (Renick u' u))) = do
    elClass "span" "sender" $ text (T.unpack u')
    elClass "span" "rename" $ text $ "is now called " ++ (T.unpack u)
    return Nothing
messageEl ( (Problem x)) = do
    elClass "span" "problem" $ text (T.unpack x)
    return Nothing

renderMessage :: MS m => ClientMessage -> m (Maybe (ES Rooma))
renderMessage = el "li" . messageEl 

-- render messages and returns new messages
messageBox :: MS m => m (ES String)
messageBox = do
        rec     t <- el "div" $ do
                        elClass "span" "talker" $  text "you say:"
                        t <- textInput $ def & setValue .~ fmap (const "") newMessage
                        return t
                focusNow t
                let newMessage =  tag (current $ value t) (textInputGetEnter t)
        return newMessage

data RoomEvent a where
    QuitRoom :: RoomEvent T.Text
    SelectRoom :: RoomEvent (Maybe T.Text)
    CreateRoom :: RoomEvent String

deriveGEq ''RoomEvent
deriveGCompare ''RoomEvent

data MessagesEvent a where
    NewMessage ::  MessagesEvent String
    EnterRoom :: MessagesEvent Rooma

deriveGEq ''MessagesEvent
deriveGCompare ''MessagesEvent

messages :: MS m => DS [ClientMessage]  -> DS String -> m (Plug MessagesEvent)
messages receivedMessages title =  divClass "message" $ do
                rec divClass "room-header" $ do
                        elClass "span" "room-title" $ dynText title
                    listen <- divClass "room-messages" $ el "ul" $ domMorph (\ms -> leftmost <$> catMaybes <$> mapM renderMessage ms) receivedMessages 
                    messageE <- divClass "room-control" $ messageBox
                return $ mergeDSums $ [EnterRoom :=> listen, NewMessage :=> messageE]

rooms :: MS m => DS [Rooma] -> m (Plug RoomEvent)
rooms roomsD = divClass "rooms-bg" $ elClass "ul" "rooms" $ flip domMorph roomsD $ \rs -> do
            root <- el "li" $ do
                        s <- (Nothing <$) <$> button "root room"
                        return $ mergeDSums [SelectRoom :=> s]
            vs <- forM rs $ \r@(Rooma n d) -> el "li" $ do 
                    s <- (Just n <$) <$> button (T.unpack d)
                    u <- (n <$) <$> button "unfollow"
                    return $ mergeDSums [QuitRoom :=> u, SelectRoom :=> s]
            new <-    el "li" $ do    
                    elClass "span" "talker" $  text "new room:"
                    rec     t <- textInput $ def & setValue .~ fmap (const "") newroom
                            let newroom = tag (current $ value t) (textInputGetEnter t)
                            focusNow t
                    return $ mergeDSums [CreateRoom :=> newroom ]
            return $ leftmost $ new :root : vs


main :: IO ()
main = mainWidget $ do
    el "style" $ text $(embedStringFile "client.css")
    rec 
        ws <- webSocket host  $ def & singleWSSend event
        -- the all messages received
        allMessages :: DS [ClientMessage] <- foldDyn (accum . accept) [] $ _webSocket_recv ws
        -- selection of parameters for acutal room 
        perRoomViewsD <- mapDyn (map $ view roomName) roomsD >>= \rooms -> combineDyn (,) allMessages actual >>= combineDyn (,) rooms
        -- messages for actual room
        receivedMessages <- mapDyn (\(rs,(ms,r)) -> foldr (partitionView rs r) [] ms) perRoomViewsD
        -- the actual room viewed
        actual  <- holdDyn Nothing $ leftmost [pick SelectRoom roomE, (Just . view roomName) <$> pick EnterRoom messagesE]
        title <- combineDyn (\x xs -> maybe "Root Room" 
                    (\y -> T.unpack . view roomDescription . fromJust $ find ((==y) . view roomName) xs) 
                    x)    
                    actual roomsD
        -- message vision
        seeRooms <- divClass "rooms-switch" $ ifNotMorph what $ button "Rooms"
        messagesE <-  ifNotMorph what $ messages receivedMessages title
        -- rooms vision
        roomE <- ifMorph what $ rooms roomsD    
        -- actual view
        what <- holdDyn False $ leftmost [False <$ pick SelectRoom roomE, True <$ seeRooms, False<$  pick CreateRoom roomE]
        -- room partecipated
        roomsD <- foldDyn ($) [] $ leftmost  
                [   (\n -> (n :) . filter (/= n)) <$> pick EnterRoom messagesE
                ,   (\n -> filter ((/= n) . view roomName))  <$> pick QuitRoom roomE
                ]
        let event =  leftmost   [ attachDynWith (\r t -> encodeMessage . show . Message r . T.pack $ t) actual $ pick NewMessage messagesE
                                , attachDynWith (\r t -> encodeMessage . show . NewRoom r . T.pack $ t) actual $  pick CreateRoom roomE
                                , encodeMessage . show . Listen . view roomName <$> pick EnterRoom messagesE 
                                , encodeMessage . show . Unlisten <$> pick QuitRoom roomE
                                ]
    return ()

  
partitionMs _ r (Regular (MessageFrom u ((==r) -> False) m)) = False
partitionMs rs _ (Regular (Room u _ dx ((`elem` rs) -> True))) = False
partitionMs _ r (Regular (Room u ((==r) -> False) dx m)) = False
partitionMs _ _ _ = True

partitionView rs r m = (if partitionMs rs r m then (m :) else id) 

accept :: ByteString -> Maybe ClientMessage
accept = readMaybe . T.unpack . decodeUtf8

accum :: Maybe a -> [a] -> [a]
accum Nothing = id
accum (Just x) = (++ [x])
