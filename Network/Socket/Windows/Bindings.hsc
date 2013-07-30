module Network.Socket.Windows.Bindings (
    updateAcceptContext,
    updateConnectContext,
    c_WSARecv,
    c_WSASend,
) where

import Network.Socket.Internal (throwSocketErrorIfMinus1_)
import Network.Socket.Windows.Types

import IOCP.Windows
import Foreign
import Foreign.C

#include <winsock2.h>

##ifdef mingw32_HOST_OS
## if defined(i386_HOST_ARCH)
##  define WINDOWS_CCONV stdcall
## elif defined(x86_64_HOST_ARCH)
##  define WINDOWS_CCONV ccall
## else
##  error Unknown mingw32 arch
## endif
##endif

foreign import WINDOWS_CCONV unsafe "winsock2.h setsockopt"
    c_setsockopt :: SOCKET
                 -> CInt        -- ^ level
                 -> CInt        -- ^ optname
                 -> Ptr CChar   -- ^ optval
                 -> CInt        -- ^ optlen
                 -> IO CInt     -- ^ 0 on success, SOCKET_ERROR otherwise

sO_UPDATE_ACCEPT_CONTEXT, sO_UPDATE_CONNECT_CONTEXT :: CInt
-- Numbers from mswsock.h in mingw-w64.
sO_UPDATE_ACCEPT_CONTEXT  = 0x700B
sO_UPDATE_CONNECT_CONTEXT = 0x7010

updateAcceptContext :: SOCKET -> SOCKET -> IO ()
updateAcceptContext acceptSock listenSock =
    with listenSock $ \listenSockPtr ->
    throwSocketErrorIfMinus1_ "setsockopt(SO_UPDATE_ACCEPT_CONTEXT)" $
    c_setsockopt acceptSock
                 (#const SOL_SOCKET) sO_UPDATE_ACCEPT_CONTEXT
                 (castPtr listenSockPtr) (fromIntegral $ sizeOf listenSock)

updateConnectContext :: SOCKET -> IO ()
updateConnectContext sock =
    throwSocketErrorIfMinus1_ "setsockopt(SO_UPDATE_CONNECT_CONTEXT)" $
    c_setsockopt sock
                 (#const SOL_SOCKET) sO_UPDATE_CONNECT_CONTEXT
                 nullPtr 0

foreign import WINDOWS_CCONV unsafe "winsock2.h WSARecv"
    c_WSARecv
      :: SOCKET       -- ^ s
      -> LPWSABUF     -- ^ lpBuffers
      -> DWORD        -- ^ dwBufferCount
      -> LPDWORD      -- ^ lpNumberOfBytesRecvd
      -> LPDWORD      -- ^ lpFlags
      -> LPOVERLAPPED -- ^ lpOverlapped
      -> LPWSAOVERLAPPED_COMPLETION_ROUTINE
                      -- ^ lpCompletionRoutine.  Must not call back
                      --   into Haskell, since 'c_WSARecv' is an
                      --   @unsafe@ foreign import.
      -> IO CInt

foreign import WINDOWS_CCONV unsafe "winsock2.h WSASend"
    c_WSASend
      :: SOCKET       -- ^ s
      -> LPWSABUF     -- ^ lpBuffers
      -> DWORD        -- ^ dwBufferCount
      -> LPDWORD      -- ^ lpNumberOfBytesSent
      -> DWORD        -- ^ dwFlags
      -> LPOVERLAPPED -- ^ lpOverlapped
      -> LPWSAOVERLAPPED_COMPLETION_ROUTINE
                      -- ^ lpCompletionRoutine.  Must not call back
                      --   into Haskell, since 'c_WSASend' is an
                      --   @unsafe@ foreign import.
      -> IO CInt
