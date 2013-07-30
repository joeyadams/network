module Network.Socket.Windows (
    associate,
    connect,
    accept,
    recv,
    send,
) where

import Data.ByteString (ByteString)
import Network.Socket.Types (Socket, SockAddr)

associate :: Socket -> IO ()
connect :: Socket -> SockAddr -> IO ()
accept :: Socket -> IO (Socket, SockAddr)
recv :: Socket -> Int -> IO ByteString
send :: Socket -> ByteString -> IO Int
