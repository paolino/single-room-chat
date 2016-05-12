{-# LANGUAGE OverloadedStrings,ViewPatterns, ScopedTypeVariables#-}

import Network.WebSockets
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Text hiding (filter)
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Exception

import Protocol
import System.Environment
import RandomName

type Env = ReaderT (TVar Text) IO

main :: IO ()
main = do
    ip:po:_ <- getArgs
    input <- newTChanIO
    output <- newBroadcastTChanIO
    forkIO . forever. atomically $ readTChan input >>= writeTChan output
    w <- worder
    runServer ip (read po) $ handleConnection w input output 
        
data Action = Accept MessageCore | ReNick | Fail Text | Exit

parse :: Text -> Protocol -> Action
parse u (Message t) = Accept (MessageFrom u t)
parse _ Rename = ReNick 

handleAction :: IO Text -> (Text -> Env ()) -> (MessageCore -> Env ()) -> Action -> Env Bool
handleAction _ _ _ Exit = return False
handleAction _ reply _ (Fail x) = do
    reply  $ pack $ show $ Problem x
    return True
handleAction newName reply send ReNick = do
    u <- liftIO newName    
    u' <- ask >>= \t -> liftIO . atomically $do
            u' <- readTVar t
            writeTVar t u
            return u'
    send (Renick u' u)
    return True

handleAction _ _ send (Accept (MessageFrom _ (T.null -> True))) = return True
handleAction _ _ send (Accept m) = send m >> return True

handleOut send tname  (e ::  ConnectionException) = atomically $ readTVar tname >>= writeTChan send . Leave

handleConnection :: IO Text -> TChan MessageCore -> TChan MessageCore -> PendingConnection -> IO ()
handleConnection newName send receive' pending = void $ do
    receive <- atomically $ dupTChan receive'
    tname <- newName >>= newTVarIO 
    connection <- acceptRequest pending
    handle (handleOut send tname) $ do
        forkIO . forever $ atomically (readTChan receive) >>= sendTextData connection . pack . show . Regular
        void $ flip runReaderT tname .forever $ do
            m <- liftIO $ receiveData connection
            case reads $ unpack m of
                [(m,_)] -> ask >>= liftIO . atomically . readTVar >>=  return . flip parse m >>= handleAction  newName
                        (liftIO . sendTextData connection) 
                        (liftIO . atomically . writeTChan send) 
                _ -> do
                    liftIO $ sendTextData connection $ pack $ show $ Problem "Garbled input"
                    return True
         
         

