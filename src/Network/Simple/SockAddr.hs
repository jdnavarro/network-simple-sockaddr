{-# LANGUAGE RankNTypes #-}

{-| This is the same API as @network-simple@ with the difference
    of working on 'SockAddr' instead of @'HostName's@.

    For a more detailed explanation check
    @<http://hackage.haskell.org/package/network-simple-0.3.0/docs/Network-Simple-TCP.html Network.Simple.TCP>@
-}

module Network.Simple.SockAddr
  (
  -- * Client side
    connect
  , connectFork
  -- * Server side
  , serve
  , listen
  , bind
  , acceptFork
  -- * Utils
  , send
  , recv
  , close
  -- * Re-exported from @Network.Socket@
  , Socket
  , SockAddr
  ) where

import Control.Monad (forever, when)
import Control.Concurrent (ThreadId, forkIO, forkFinally)
import Data.ByteString (ByteString)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Directory (removeFile)
import qualified Network.Socket as NS
import Network.Socket
  ( SockAddr(SockAddrInet, SockAddrInet6, SockAddrUnix)
  , SocketType(Stream)
  , Family(AF_INET, AF_INET6, AF_UNIX)
  , Socket
  , defaultProtocol
  )
import qualified Network.Socket.ByteString as NSB
import Control.Monad.Catch (MonadCatch, bracket, bracketOnError, throwM)

-- * Client side

{-| Connect to a server and use the connection.

    The connection socket is closed when done or in case of exceptions.
-}
connect :: (MonadIO m, MonadCatch m)
        => SockAddr
        -- ^ Server address.
        -> (Socket -> m r)
        -- ^ Computation taking the socket connection socket.
        -> m r
connect addr = bracket connect' (liftIO . NS.close)
  where
    connect' = do sock <- newSocket addr
                  liftIO $ NS.connect sock addr
                  return sock

-- | Like 'connect' but fork the connection in a different thread.
connectFork :: MonadIO m
            => SockAddr
            -- ^ Server address.
            -> (forall m' . (Functor m', MonadIO m', MonadCatch m')
                         => Socket -> m' ())
            -- ^ Computation taking the socket connection socket.
            -> m ThreadId
connectFork addr k = liftIO . forkIO $ connect addr k
{-# INLINABLE connectFork #-}

-- * Server side

{-| Start a server that accepts incoming connections and handles them
    concurrently in different threads.

    Any acquired network resources are properly closed and discarded when done
    or in case of exceptions.
-}
serve :: (MonadIO m, MonadCatch m)
      => SockAddr
      -- ^ Address to bind to.
      -> (forall m' . (Functor m', MonadIO m', MonadCatch m')
                   => SockAddr -> Socket -> m' ())
      -- ^ Computation to run in a different thread
      --   once an incoming connection is accepted. Takes the
      --   the remote end address and the connection socket.
      -> m ()
serve addr k = listen addr $ \sock -> forever $ acceptFork sock k

{-| Bind a listening socket and use it.

    The listening socket is closed when done or in case of exceptions.
-}
listen :: (MonadIO m, MonadCatch m)
       => SockAddr
       -- ^ Address to bind to.
       -> (Socket -> m r)
       -- ^ Computation taking the listening socket.
       -> m r
listen addr = bracket listen' (close addr)
  where
    listen' = liftIO $ do sock <- bind addr
                          NS.listen sock $ max 2048 NS.maxListenQueue
                          return sock
{-| Obtain a 'Socket' bound to the given 'SockAddr'.

   The obtained 'Socket' should be closed manually using 'close' when it's not
   needed anymore.

   Prefer to use 'listen' if you will be listening on this socket and using it
   within a limited scope, and would like it to be closed immediately after its
   usage or in case of exceptions.
-}
bind :: (MonadIO m, MonadCatch m) => SockAddr -> m Socket
bind addr = bracketOnError (newSocket addr) (close addr)
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

{-| Accept a single incoming connection and use it in a different thread.

    The connection socket is closed when done or in case of exceptions.
-}
acceptFork :: (MonadIO m, MonadCatch m)
           => Socket
           -- ^ Listening and bound socket.
           -> (forall m' . (Functor m', MonadIO m', MonadCatch m')
                        => SockAddr -> Socket -> m' ())
           -- ^ Computation to run in a different thread
           --   once an incoming connection is accepted. Takes the
           --   remote end address and connection socket.
           -> m ThreadId
acceptFork lsock k = liftIO $ do
    (csock,caddr) <- NS.accept lsock
    forkFinally (k caddr csock)
                (\ea -> NS.close csock >> either throwM return ea)
{-# INLINABLE acceptFork #-}

-- * Utils

-- | Writes the given bytes to the socket.
send :: MonadIO m => Socket -> ByteString -> m ()
send sock bs = liftIO $ NSB.sendAll sock bs
{-# INLINE send #-}

-- | Read up to a limited number of bytes from a socket.
recv :: MonadIO m => Socket -> Int -> m ByteString
recv sock n = liftIO $ NSB.recv sock n
{-# INLINE recv #-}

-- | Close the 'Socket' and unlinks the 'SockAddr' for Unix sockets.
close :: MonadIO m => SockAddr -> Socket -> m ()
close (SockAddrUnix path) sock = liftIO $ NS.close sock >> removeFile path
close _                   sock = liftIO $ NS.close sock
{-# INLINE close #-}

-- * Internal

newSocket :: MonadIO m => SockAddr -> m Socket
newSocket addr = liftIO $ NS.socket (fam addr) Stream defaultProtocol
  where
    fam (SockAddrInet  {}) = AF_INET
    fam (SockAddrInet6 {}) = AF_INET6
    fam (SockAddrUnix  {}) = AF_UNIX
{-# INLINABLE newSocket #-}
