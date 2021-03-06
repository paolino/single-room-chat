{-# LANGUAGE OverloadedStrings,ViewPatterns, ScopedTypeVariables#-}

import Network.WebSockets
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Text
import qualified Data.Text as T
import Control.Monad.State

import Protocol
import System.Environment


type Env = StateT (Maybe Text) IO

main :: IO ()
main = do
    ip:po:_ <- getArgs
    input <- newTChanIO
    output <- newBroadcastTChanIO
    forkIO . forever. atomically $ readTChan input >>= writeTChan output
    runServer ip (read po) $ handleConnection input output 
        


data Action = Accept MessageCore | SetNick (Maybe Text) | Fail Text | Exit

parse :: Maybe Text -> Protocol -> Action
parse _ Logout = SetNick Nothing
parse (Just u) (Message t) = Accept (MessageFrom u t)
parse Nothing (Message t) = Fail "Set your nick!"
parse _ (Login u) = SetNick (Just u)

handleAction :: (Text -> Env ()) -> (MessageCore -> Env ()) -> Action -> Env Bool
handleAction _ _ Exit = return False
handleAction reply _ (Fail x) = do
    reply  $ pack $ show $ Problem x
    return True
handleAction _ send (SetNick u) = do
    r <- get
    case r of 
        Nothing -> case u of
            Just u -> send (Join u)
            Nothing -> return ()
        Just u' -> do
            case u of
                Just u -> send (Renick u' u)
                Nothing -> send (Leave u')
    put u
    return True
handleAction _ send (Accept (MessageFrom _ (T.null -> True))) = return True
handleAction _ send (Accept m) = send m >> return True




handleConnection :: TChan MessageCore -> TChan MessageCore -> PendingConnection -> IO ()
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
         

