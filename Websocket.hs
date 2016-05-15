{-# LANGUAGE OverloadedStrings,ViewPatterns, ScopedTypeVariables,TemplateHaskell#-}
module Websocket where

import Network.WebSockets
import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Data.Text hiding (filter)
import qualified Data.Text as T
import Control.Monad.Reader
import Control.Exception
import Control.Lens
import Control.Lens.TH
import Protocol
import System.Environment
import RandomName

data UserState = UserState {
    _listens :: [Text],
    _name :: Text
    }

makeLenses ''UserState

type Env = ReaderT (TVar UserState) IO

        
handleAction :: IO Text -> (Text -> Env ()) -> (Input -> Env ()) -> Output -> Env ()
handleAction newName _ send (NewRoom mr desc) = do
    r <- liftIO newName
    u <- ask >>= \t -> liftIO $ atomically $ do
            modifyTVar t . over listens $ (:) r
            view name <$> readTVar t
    send (Room u mr desc r)
handleAction _ _ _ (Listen r) = ask >>= \t -> liftIO . atomically . modifyTVar t . over listens $ (:) r
handleAction _ _ _ (Unlisten r) = ask >>= \t -> liftIO . atomically . modifyTVar t . over listens $ filter (/= r)

handleAction _ _ send (Message _ (T.null -> True)) = return ()
handleAction _ _ send (Message mr t) = do
    u <- ask >>= \t -> liftIO $ atomically $ view name <$> readTVar t
    send (MessageFrom u mr t) 

handleOut send state  (e ::  ConnectionException) = atomically $ readTVar state >>= writeTChan send . Leave . view name

handleConnection :: Text -> IO Text ->  TChan Input -> TChan Input -> PendingConnection -> IO ()
handleConnection name newName send receive' pending = void $ do
    receive <- atomically $ dupTChan receive'
    state <- newTVarIO . UserState [] $ name
    atomically $ writeTChan send (Join name)
    connection <- acceptRequest pending
    handle (handleOut send state) $ do
        forkIO . forever . join $ atomically $ do 
            m <- readTChan receive
            ls <- view listens <$> readTVar state
            let t = case m of 
                        MessageFrom _ (Just r) _ -> r `elem` ls
                        _ -> True
            if t then return (sendTextData connection . pack . show . Regular $ m) else return (return ())
                    
                    
                    
        void $ flip runReaderT state .forever $ do
            m <- liftIO $ receiveData connection
            case reads $ unpack m of
                [(m,_)] -> handleAction newName
                        (liftIO . sendTextData connection) 
                        (liftIO . atomically . writeTChan send) m
                _ -> liftIO $ sendTextData connection $ pack $ show $ Problem "Garbled input"
         
         

-- awfully database identities in memory


application     :: TVar Text -- session cookie
                -> IO ServerApp
application tcookie = do
    input <- newTChanIO -- global incoming messages channel
    fanning <- newBroadcastTChanIO -- global fanning channel
    memory <- newTVarIO [] -- in memory linear database of cookie -> user names
    forkIO . forever. atomically $ 
        readTChan input >>= writeTChan fanning -- fanner thread
    w <- worder -- get a function to create random names
    return $ \conn -> do -- return an IO action
        newname <- w -- always create a new name on new connection
        name <- atomically $ do -- resolve to old name if cookie is known
            coockie <- readTVar tcookie
            mem <- readTVar memory
            case resolve newname coockie mem of
                Left name -> return name
                Right mem' -> writeTVar memory mem' >> return newname
        handleConnection name w input fanning conn


resolve :: Text -> Text -> [(Text,Text)] -> Either Text [(Text,Text)]
resolve new input mem = case lookup input mem of
    Nothing -> Right $ Prelude.take 1000 $ (input,new):mem
    Just old -> Left old
