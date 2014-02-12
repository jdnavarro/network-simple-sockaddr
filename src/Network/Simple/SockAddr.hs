{-# LANGUAGE RankNTypes #-}
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

import Control.Monad (forever, when)
import Control.Concurrent (ThreadId, forkIO, forkFinally)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (MonadIO, liftIO)
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
import Control.Monad.Catch (MonadCatch, bracket, bracketOnError, throwM)

serve :: (MonadIO m, MonadCatch m)
      => SockAddr
      -> (forall m' . MonadIO m' => SockAddr -> Socket -> m' ())
      -> m ()
serve addr k = listen addr $ \sock -> forever $ acceptFork sock k

listen :: (MonadIO m, MonadCatch m) => SockAddr -> (Socket -> m r) -> m r
listen addr = bracket listen' (liftIO . NS.close)
  where
    listen' = liftIO $ do sock <- bind addr
                          NS.listen sock $ max 2048 NS.maxListenQueue
                          return sock

bind :: (MonadIO m, MonadCatch m) => SockAddr -> m Socket
bind addr = bracketOnError (newSocket addr) (liftIO . NS.close)
          $ \sock -> liftIO $ do
                let set so n = when (NS.isSupportedSocketOption so)
                                    (NS.setSocketOption sock so n)
                when (isTCP addr) (set NS.NoDelay 1)
                set NS.ReuseAddr 1
                NS.bindSocket sock addr
                return sock
  where
    isTCP (SockAddrUnix {}) = False
    isTCP _                 = True

acceptFork :: (MonadIO m, MonadCatch m)
           => Socket
           -> (forall m' . MonadIO m' => SockAddr -> Socket -> m' ())
           -> m ThreadId
acceptFork lsock k = liftIO $ do
    (csock,caddr) <- NS.accept lsock
    forkFinally (k caddr csock)
                (\ea -> NS.close csock >> either throwM return ea)

connect :: (MonadIO m, MonadCatch m) => SockAddr -> (Socket -> m r) -> m r
connect addr = bracket connect' (liftIO . NS.close)
  where
    connect' = do sock <- newSocket addr
                  liftIO $ NS.connect sock addr
                  return sock

connectFork :: MonadIO m
            => SockAddr
            -> (forall m' . MonadIO m' => Socket -> m' ())
            -> m ThreadId
connectFork addr k = liftIO . forkIO $ connect addr k

newSocket :: MonadIO m => SockAddr -> m Socket
newSocket addr = liftIO $ NS.socket (fam addr) Stream defaultProtocol
  where
    fam (SockAddrInet  {}) = AF_INET
    fam (SockAddrInet6 {}) = AF_INET6
    fam (SockAddrUnix  {}) = AF_UNIX

send :: MonadIO m => Socket -> ByteString -> m ()
send sock bs = liftIO $ NSB.sendAll sock bs
