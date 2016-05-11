{-# LANGUAGE RecursiveDo, GADTs, TemplateHaskell,NoMonomorphismRestriction,OverloadedStrings,FlexibleContexts #-}
import Reflex.Dom
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Monoid
import Protocol
import Data.FileEmbed
import Lib (ES,MS,DS,BS,domMorph,domMorph',ifMorph,mergeDSums,pick,Plug)
import Text.Read (readMaybe)
import Data.ByteString (ByteString)
import Data.Dependent.Map 
import Data.GADT.Compare.TH
import GHCJS.DOM.Element (focus)
import Control.Monad (void)
import Control.Monad.Trans (liftIO)
import Control.Lens

performArg :: MonadWidget t m => (b -> IO a) -> Event t b -> m (Event t a)
performArg f x = performEvent (fmap (liftIO . f) x)

focusNow t = getPostBuild >>= performArg (const $ focus (t ^. textInput_element))

data MessageE a where
    NewMessage :: MessageE ByteString
    LogoutE :: MessageE ()

deriveGEq ''MessageE
deriveGCompare ''MessageE


encodeMessage = encodeUtf8 . T.pack 

messageEl (Just (Regular (MessageFrom u m))) = do
    elClass "span" "sender" $ text (T.unpack u)
    elClass "span" "content" $ text (T.unpack m)
messageEl (Just (Regular (Join u))) = do
    elClass "span" "sender" $ text (T.unpack u)
    elClass "span" "action" $ text "has joined"
messageEl (Just (Regular (Leave u))) = do
    elClass "span" "sender" $ text (T.unpack u)
    elClass "span" "action" $ text "has leaved"
messageEl (Just (Regular (Renick u' u))) = do
    elClass "span" "sender" $ text (T.unpack u')
    elClass "span" "rename" $ text $ "is now called " ++ (T.unpack u)
messageEl (Just (Problem x)) = do
    elClass "span" "problem" $ text (T.unpack x)
messageEl Nothing = elClass "span" "error" $ text $ "--"

renderMessage :: MS m => ByteString -> m ()
renderMessage = el "li" . messageEl . readMaybe . T.unpack . decodeUtf8

-- render messages and returns new messages
messageBox :: MS m =>  DS String  -> m (Plug MessageE)
messageBox  du = do
        rec     
                (t,logoutE) <- do
                    elClass "span" "talker" $ dynText du >> text " says:"
                    t <- textInput $ def & setValue .~ fmap (const "") newMessage
                    logoutE <- button "ReNick"
                    return (t,logoutE)
                focusNow t
                let newMessage = fmap (encodeMessage . ("Message " ++) . show) $ tag (current $ value t) $ textInputGetEnter t
        return $ mergeDSums [NewMessage :=> newMessage, LogoutE :=> logoutE]


singleWSSend :: Reflex t => Event t ByteString -> WebSocketConfig t -> WebSocketConfig t
singleWSSend m =  webSocketConfig_send .~ (return <$> m)

main :: IO ()
main = mainWidget $ do
    el "style" $ text $(embedStringFile "client.css")
    divClass "message" $ do
            rec el "ul" $ domMorph' (mapM_ renderMessage ) receivedMessages 
                messageE <- ifMorph loggedD $ messageBox userD
                (userE,loggedD,userD) <-  do
                    rec     loginE <- mapDyn not loggedD >>= \nl -> ifMorph nl $ do  
                                rec t <- do 
                                       elClass "span" "talker" $ text "Nick"
                                       textInput $ def & setValue .~ fmap (const "") loginE
                                    let  loginE = tag (current $ value t) $ textInputGetEnter t
                                    focusNow t
                                return loginE
                            loggedD <- holdDyn False $ leftmost [True <$ loginE, False <$ pick LogoutE messageE]
                    userD <- holdDyn "" loginE
                    let userE = (encodeMessage . ("Login " ++) . show) <$> loginE
                    return $ (userE, loggedD,userD)

                ws <- webSocket "ws://lambdasistemi.net:50100" $ def & singleWSSend (leftmost [pick NewMessage messageE,userE])
                receivedMessages <- foldDyn (\m ms -> ms ++ [m]) [] $ _webSocket_recv ws
            return ()
