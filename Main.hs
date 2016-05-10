{-# LANGUAGE OverloadedStrings,ViewPatterns, ScopedTypeVariables#-}

import Network.WebSockets
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Text
import Control.Monad.State

import Protocol


type Env = StateT (Maybe Text) IO

main :: IO ()
main = do
    input <- newTChanIO
    output <- newBroadcastTChanIO
    forkIO . forever. atomically $ readTChan input >>= writeTChan output
    runServer "127.0.0.1" 50100 $ handleConnection input output 
        


data Action = Accept MessageFrom | SetNick (Maybe Text) | Fail Text | Exit

parse :: Maybe Text -> Protocol -> Action
parse _ Logout = SetNick Nothing
parse (Just u) (Message t) = Accept (MessageFrom u t)
parse Nothing (Message t) = Fail "Set your nick!"
parse _ (Login u) = SetNick (Just u)

handleAction :: (Text -> Env ()) -> (MessageFrom -> Env ()) -> Action -> Env Bool
handleAction _ _ Exit = return False
handleAction reply _ (Fail x) = do
    reply  $ pack $ show $ Problem x
    return True
handleAction _ _ (SetNick u) = modify (const u) >> return True
handleAction _ send (Accept m) = send m >> return True




handleConnection :: TChan MessageFrom -> TChan MessageFrom -> PendingConnection -> IO ()
handleConnection send receive' pending = void $ do
  receive <- atomically $ dupTChan receive'
  connection <- acceptRequest pending
  forkIO . forever $ atomically (readTChan receive) >>= sendTextData connection . pack . show . Regular
  flip runStateT Nothing .forever $ do
        m <- liftIO $ receiveData connection
        case reads $ unpack m of
            [(m,_)] -> gets (flip parse m) >>= handleAction 
                    (liftIO . sendTextData connection) 
                    (liftIO . atomically . writeTChan send) 
            _ -> do
                liftIO $ sendTextData connection $ pack $ show $ Problem "Garbled input"
                return True
         

