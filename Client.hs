{-# LANGUAGE RecursiveDo, TemplateHaskell,NoMonomorphismRestriction,OverloadedStrings #-}
import Reflex.Dom
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Monoid
import Protocol
import Data.FileEmbed
import Lib
import Text.Read

encodeMessage = (:[]) . encodeUtf8 . T.pack 

messageEl (Just (Regular (MessageFrom u m))) = do
    elClass "span" "sender" $ text (T.unpack u)
    elClass "span" "content" $ text (T.unpack m)
messageEl (Just (Problem x)) = do
    elClass "span" "problem" $ text (T.unpack x)
messageEl Nothing = elClass "span" "error" $ text $ "--"

parseMessage = readMaybe . T.unpack . decodeUtf8

main :: IO ()
main = mainWidget $ do
    el "style" $ text $(embedStringFile "client.css")
    user <- divClass "login" $ do
        rec elClass "p" "head" $ 
                    holdDyn "Nick unset" (leftmost [("You are " ++) <$> loginE, "Nick unset" <$ logout]) >>= dynText
            (newLogin,loginE,logout) <- do  
                    rec     t <- do 
                                text "Nick:"
                                textInput $ def & setValue .~ fmap (const "") newLogin
                            let     newLogin = fmap (encodeMessage . ("Login " ++) . show) loginE
                                    loginE = tag ( current (value t)) $ textInputGetEnter t
                            logout <- ((encodeMessage "Logout") <$)  <$> button "Logout"
                    return (newLogin, loginE, logout)
        return $ leftmost [newLogin,logout]
    divClass "message" $ do
        elClass "p" "head" $ text "Messaging"
        rec     ws <- webSocket "ws://lambdasistemi.net:50100" $ def & webSocketConfig_send .~ leftmost [newMessage,user]
                receivedMessages <- foldDyn (\m ms -> tail ms ++ [m]) (replicate 10 "") $ _webSocket_recv ws
                el "ul" $ simpleList receivedMessages $ \m -> el "li" $ mapDyn parseMessage m >>= domMorph ((never <$) . messageEl)
                t <- do
                    text "Send:"
                    textInput $ def & setValue .~ fmap (const "") newMessage
                let newMessage = fmap (encodeMessage . ("Message " ++) . show) $ tag (current $ value t) $ textInputGetEnter t
        return ()
