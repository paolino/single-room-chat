{-# LANGUAGE OverloadedStrings,ViewPatterns, ScopedTypeVariables#-}

import Network.WebSockets
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Text hiding (filter)
import qualified Data.Text as T
import Control.Monad.State
import Control.Exception

import Protocol
import System.Environment


type Env = StateT (Maybe Text) IO

main :: IO ()
main = do
    ip:po:_ <- getArgs
    input <- newTChanIO
    output <- newBroadcastTChanIO
    forkIO . forever. atomically $ readTChan input >>= writeTChan output
    names <- newTVarIO []
    runServer ip (read po) $ handleConnection names input output 
        


data Action = Accept MessageCore | SetNick (Maybe Text) | Fail Text | Exit

parse :: Maybe Text -> Protocol -> Action
parse _ Logout = SetNick Nothing
parse (Just u) (Message t) = Accept (MessageFrom u t)
parse Nothing (Message t) = Fail "Set your nick!"
parse _ (Login u) = SetNick (Just u)

handleAction :: TVar [Text] -> (Text -> Env ()) -> (MessageCore -> Env ()) -> Action -> Env Bool
handleAction _ _ _ Exit = return False
handleAction _ reply _ (Fail x) = do
    reply  $ pack $ show $ Problem (GenericProblem x)
    return True
handleAction names reply send (SetNick u) = do
    r <- get
    case r of 
        Nothing -> case u of
            Just u -> send (Join u)
            Nothing -> return ()
        Just u' -> do
            case u of
                Just u -> do
                    result <- liftIO . atomically $ do
                        ns <- readTVar names
                        if u `elem` ns then return False
                        else  do
                            writeTVar names $ u : filter ((/=) u') ns
                            return True
                    if result then send (Renick u' u)
                        else reply $ pack $ show $ Problem (NameTaken u)

                Nothing -> send (Leave u')
    put u
    return True
handleAction _ _ send (Accept (MessageFrom _ (T.null -> True))) = return True
handleAction _ _ send (Accept m) = send m >> return True


filterNames Nothing = id
filterNames (Just u) = filter (/= u)

handleOut send tname names (e ::  ConnectionException) = atomically $ do 
    mn <- readTVar tname 
    case mn of
        Just n -> do    modifyTVar names $ filter (/= n)
                        writeTChan send (Leave n)
        Nothing -> return ()
         


handleConnection :: TVar [Text] -> TChan MessageCore -> TChan MessageCore -> PendingConnection -> IO ()
handleConnection names send receive' pending = void $ do
    receive <- atomically $ dupTChan receive'
    tname <- newTVarIO Nothing
    connection <- acceptRequest pending
    handle (handleOut send tname names) $ do
        forkIO . forever $ atomically (readTChan receive) >>= sendTextData connection . pack . show . Regular
        void $ flip runStateT Nothing .forever $ do
            m <- liftIO $ receiveData connection
            case reads $ unpack m of
                [(m,_)] -> gets (flip parse m) >>= handleAction names 
                        (liftIO . sendTextData connection) 
                        (liftIO . atomically . writeTChan send) 
                _ -> do
                    liftIO $ sendTextData connection $ pack $ show $ Problem (GenericProblem "Garbled input")
                    return True
         
         

