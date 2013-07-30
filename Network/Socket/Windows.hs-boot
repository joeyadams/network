module Network.Socket.Windows (
    associate,
    connect,
    accept,
    recvBuf,
    sendBuf,
) where

import Data.ByteString (ByteString)
import Foreign.Ptr (Ptr)
import Network.Socket.Types (Socket, SockAddr)

associate :: Socket -> IO ()
connect :: Socket -> SockAddr -> IO ()
accept :: Socket -> IO (Socket, SockAddr)
recvBuf :: Socket -> Ptr a -> Int -> IO Int
sendBuf :: Socket -> Ptr a -> Int -> IO Int
