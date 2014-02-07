module Network.Simple.SockAddr
  ( Socket
  , SockAddr
  , serve
  , listen
  , bind
  , acceptFork
  , connect
  , connectFork
  , send
  , recv
  ) where

import Control.Monad (forever)
import Control.Exception (bracket, bracketOnError, throwIO)
import Control.Concurrent (ThreadId, forkIO, forkFinally)
import Data.ByteString (ByteString)
import qualified Network.Socket as NS
import Network.Socket
  ( SockAddr(SockAddrInet, SockAddrInet6, SockAddrUnix)
  , SocketType(Stream)
  , Family(AF_INET, AF_INET6, AF_UNIX)
  , Socket
  , defaultProtocol
  )
import Network.Socket.ByteString (recv)
import qualified Network.Socket.ByteString as NSB

serve :: SockAddr -> (Socket -> IO ()) -> IO ()
serve addr k = listen addr $ \sock -> forever $ acceptFork sock k

listen :: SockAddr -> (Socket -> IO r) -> IO r
listen addr = bracket listen' NS.close
  where
    listen' = do sock <- bind addr
                 NS.listen sock $ max 2048 NS.maxListenQueue
                 return sock

bind :: SockAddr -> IO Socket
bind addr = bracketOnError (newSocket addr) NS.close $ \sock -> do
    NS.setSocketOption sock NS.NoDelay 1
    NS.setSocketOption sock NS.ReuseAddr 1
    NS.bindSocket sock addr
    return sock

acceptFork :: Socket -> (Socket -> IO ()) -> IO ThreadId
acceptFork lsock k = do
    (csock,_) <- NS.accept lsock
    forkFinally (k csock) (\ea -> NS.close csock >> either throwIO return ea)

connect :: SockAddr -> (Socket -> IO r) -> IO r
connect addr = bracket connect' NS.close
  where
    connect' = do sock <- newSocket addr
                  NS.connect sock addr
                  return sock

connectFork :: SockAddr -> (Socket -> IO ()) -> IO ThreadId
connectFork addr k = forkIO $ connect addr k

newSocket :: SockAddr -> IO Socket
newSocket (SockAddrInet  {}) = NS.socket AF_INET  Stream defaultProtocol
newSocket (SockAddrInet6 {}) = NS.socket AF_INET6 Stream defaultProtocol
newSocket (SockAddrUnix  {}) = NS.socket AF_UNIX  Stream defaultProtocol

send :: Socket -> ByteString -> IO ()
send = NSB.sendAll
