{-# LANGUAGE OverloadedStrings #-}
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
import Network.WebSockets.Snap
import Snap.Core
import Snap.Http.Server
import Control.Applicative
import Websocket (application)

main :: IO ()
main = do
    m <- newTVarIO ""
    ws <- liftIO (application m)
    quickHttpServe (site ws)

getsRequest $ fmap (decodeUtf8 . cookieValue) . Data.List.find ((== "id") . cookieName) . rqCookies 


rqCookies 
site :: (PendingConnection -> IO ()) -> Snap ()
site wsApp = 
    ifTop (writeBS "hello world") <|>
    route 
          [ ("foo", writeBS "bar")
          , ("echo/:echoparam", echoHandler)
          , ("/ws", runWebSocketsSnap wsApp) 
          ]

echoHandler :: Snap ()

echoHandler = do
    param <- getParam "echoparam"
    maybe (writeBS "must specify echo/param in URL")
          writeBS param


