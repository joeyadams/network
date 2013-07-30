{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
-- | IOCP replacement implementations
module Network.Socket.Windows (
    associate,
    connect,
    accept,
    recvBuf,
    sendBuf,
) where

import Network.Socket.Windows.Bindings
import Network.Socket.Windows.Mswsock
import Network.Socket.Windows.Types
import Network.Socket.Types

import IOCP.Manager (Completion(..))
import qualified IOCP.Manager as M
import IOCP.Windows

import Control.Concurrent.MVar
import Control.Exception
import Control.Monad
import Foreign
import Foreign.C
import GHC.IO.Exception
import Network.Socket hiding (connect, accept, recvBuf, sendBuf)
import qualified Network.Socket as NS
import Network.Socket.Internal
    ( withSockAddr
    , peekSockAddr
    , sizeOfSockAddrByFamily
    , throwSocketErrorIfMinus1_
    )


-- | Associate an existing 'Socket' with the I\/O manager.  This step must be
-- performed before any of the I\/O functions in this module may be used.
-- Calling 'associate' on an already-associated handle will throw an 'IOError'.
--
-- 'Network.Socket.socket' calls this.
associate :: Socket -> IO ()
associate = M.associate . sockHANDLE

-- | 'Network.Socket.connect' calls this.
connect :: Socket -> SockAddr -> IO ()
connect sock addr =
    join $ modifyMVar (sockStatus sock) $ \status ->
    case status of
        NotConnected ->
            -- Relinquish the MVar before calling 'bind', or it will deadlock.
            return (status, bindAnyPort sock >> connect sock addr)
        Bound -> do
            rawConnect (sockSOCKET sock) addr
            return (Connected, return ())
        _ -> fail $ "connect: can't peform connect on socket in status " ++ show status

-- | @ConnectEx()@ requires the socket to be bound to a local address already,
-- while @connect()@ does this step automatically.  Try to emulate the implicit
-- bind done by @connect()@.
bindAnyPort :: Socket -> IO ()
bindAnyPort sock =
  case sockFamily sock of
      AF_INET  -> bind sock $ SockAddrInet aNY_PORT iNADDR_ANY
      AF_INET6 -> bind sock $ SockAddrInet6 aNY_PORT 0 iN6ADDR_ANY 0
                  -- TODO: test AF_INET6
      family ->
          -- Don't know how to perform implicit bind for other
          -- address families.
          throwUnsupported "connect" $
              "address family " ++ show family ++ " not supported"

-- | 'Network.Socket.accept' calls this.
accept :: Socket -> IO (Socket, SockAddr)
accept sock = do
    status <- readMVar (sockStatus sock)
    -- Network.Socket.accept also allows the Connected state, but I don't know
    -- when that would work, so not supporting it for now.
    if status == Listening
      then do
          newSock <- socket (sockFamily sock) (sockType sock) (sockProtocol sock)
          let !sz = sizeOfSockAddrByFamily (sockFamily sock)
          (_localAddr, remoteAddr) <- rawAccept (sockSOCKET sock) (sockSOCKET newSock) sz sz
          _ <- swapMVar (sockStatus newSock) Connected
          return (newSock, remoteAddr)
      else fail $ "accept: can't perform accept on socket in status " ++ show status

-- | Call @ConnectEx@, which requires the socket to be initially bound.
rawConnect :: SOCKET -> SockAddr -> IO ()
rawConnect sock addr = do
    Mswsock{..} <- getMswsock
    withSockAddr addr $ \ptr len ->
      withOverlapped_ "connect" sock isFALSE $
        mswConnectEx sock ptr (fromIntegral len) nullPtr 0 nullPtr

    -- Only a limited set of operations are available on a socket newly
    -- connected with ConnectEx.  Set SO_UPDATE_CONNECT_CONTEXT so we can do
    -- things like shutdown() and getpeername().
    updateConnectContext sock

-- | Call @AcceptEx@, which returns both the local and remote addresses.
rawAccept :: SOCKET -- ^ Socket that 'listen' was called on
          -> SOCKET -- ^ Socket that will be used to talk to the server
          -> Int    -- ^ Local address maximum length
          -> Int    -- ^ Remote address maximum length
          -> IO (SockAddr, SockAddr)
rawAccept listenSock acceptSock localAddrLen remoteAddrLen = do
    Mswsock{..} <- getMswsock
    allocaBytes (localAddrLen + remoteAddrLen + 32) $ \buf ->
      with nullPtr $ \localAddrPtr ->
      with nullPtr $ \remoteAddrPtr ->
      with (fromIntegral localAddrLen) $ \localAddrLenPtr ->
      with (fromIntegral remoteAddrLen) $ \remoteAddrLenPtr -> do
          withOverlapped_ "accept" listenSock isFALSE $
              mswAcceptEx listenSock acceptSock buf
                          0 (fi $ localAddrLen + 16) (fi $ remoteAddrLen + 16)
                          nullPtr
          mswGetAcceptExSockaddrs buf
              0 (fi $ localAddrLen + 16) (fi $ remoteAddrLen + 16)
              localAddrPtr localAddrLenPtr
              remoteAddrPtr remoteAddrLenPtr
          localAddr <- peek localAddrPtr >>= peekSockAddr
          remoteAddr <- peek remoteAddrPtr >>= peekSockAddr

          -- Only a limited set of operations are available on a socket newly
          -- accepted with AcceptEx.  Set SO_UPDATE_ACCEPT_CONTEXT so we can do
          -- things like shutdown() and getpeername().
          updateAcceptContext acceptSock listenSock

          return (localAddr, remoteAddr)

-- | Unlike 'Network.Socket.recvBuf' (which calls this), return 0 on EOF
-- instead of throwing an exception.
recvBuf :: Socket -> Ptr a -> Int -> IO Int
recvBuf sock ptr nbytes
  | nbytes < 0 = throwInvalidArgument "recv" "non-positive length"
  | otherwise  = rawRecv (sockSOCKET sock) [(ptr, nbytes)]

sendBuf :: Socket -> Ptr a -> Int -> IO Int
sendBuf sock ptr nbytes = rawSend (sockSOCKET sock) [(ptr, nbytes)]

rawRecv :: SOCKET -> [(Ptr a, Int)] -> IO Int
rawRecv sock bufs =
    withWSABUFs bufs $ \bufPtr bufCount ->
    with 0 $ \lpFlags ->
    withOverlappedNB "recv" sock (/= 0) $ \ol ->
    c_WSARecv sock bufPtr bufCount nullPtr lpFlags ol nullFunPtr

rawSend :: SOCKET -> [(Ptr a, Int)] -> IO Int
rawSend sock bufs =
    withWSABUFs bufs $ \bufPtr bufCount ->
    withOverlappedNB "send" sock (/= 0) $ \ol ->
    c_WSASend sock bufPtr bufCount nullPtr 0 ol nullFunPtr

------------------------------------------------------------------------
-- withOverlapped wrappers for SOCKET

withOverlapped :: String -> SOCKET -> (a -> Bool) -> (LPOVERLAPPED -> IO a) -> IO Completion
withOverlapped loc s = M.withOverlapped loc (toHANDLE s)

withOverlapped_ :: String -> SOCKET -> (a -> Bool) -> (LPOVERLAPPED -> IO a) -> IO ()
withOverlapped_ loc sock p s = do
    Completion{..} <- withOverlapped loc sock p s
    if cError /= 0
      then throwErrCode loc cError
      else return ()

withOverlappedNB :: String -> SOCKET -> (a -> Bool) -> (LPOVERLAPPED -> IO a) -> IO Int
withOverlappedNB loc sock p s = do
    Completion{..} <- withOverlapped loc sock p s
    if cError /= 0
      then throwErrCode loc cError
      else return $! fromIntegral cNumBytes

------------------------------------------------------------------------
-- Socket record accessors

sockSOCKET :: Socket -> SOCKET
sockSOCKET = SOCKET . fromIntegral . sockFd

sockHANDLE :: Socket -> HANDLE
sockHANDLE = toHANDLE . sockSOCKET

------------------------------------------------------------------------
-- Utilities

throwUnsupported :: String -> String -> IO a
throwUnsupported loc descr =
    throwIO IOError
            { ioe_handle      = Nothing
            , ioe_type        = UnsupportedOperation
            , ioe_location    = loc
            , ioe_description = descr
            , ioe_errno       = Nothing
            , ioe_filename    = Nothing
            }

throwInvalidArgument :: String -> String -> IO a
throwInvalidArgument loc descr =
    throwIO IOError
            { ioe_handle      = Nothing
            , ioe_type        = InvalidArgument
            , ioe_location    = loc
            , ioe_description = descr
            , ioe_errno       = Nothing
            , ioe_filename    = Nothing
            }

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral
{-# INLINE fi #-}
