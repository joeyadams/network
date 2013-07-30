{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Network.Socket.Windows.Types (
    SOCKET(..),
    iNVALID_SOCKET,
    toHANDLE,

    -- * WSABUF
    WSABUF(..),
    LPWSABUF,
    withWSABUFs,

    -- * Miscellaneous
    LPWSAOVERLAPPED,
    LPWSAOVERLAPPED_COMPLETION_ROUTINE,
    WSAOVERLAPPED_COMPLETION_ROUTINE,
) where

import IOCP.Windows

import Data.Typeable (Typeable)
import Data.Word
import Foreign
import Foreign.C.Types

#include <winsock2.h>

#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

-- | /This/ is the correct type for socket, not CInt.  On Windows,
-- SOCKET is UINT_PTR, meaning it's 64 bits in 64-bit programs.
newtype SOCKET = SOCKET (#type SOCKET)
    deriving (Eq, Ord, Show, Storable, Typeable)

iNVALID_SOCKET :: SOCKET
iNVALID_SOCKET = SOCKET #const INVALID_SOCKET

toHANDLE :: SOCKET -> HANDLE
toHANDLE (SOCKET s) = wordPtrToPtr (fromIntegral s)

data WSABUF = WSABUF
    { wbLen :: !CULong
    , wbBuf :: !(Ptr CChar)
    }
  deriving (Eq, Show)

type LPWSABUF = Ptr WSABUF

instance Storable WSABUF where
    sizeOf    _ = (#size      WSABUF)
    alignment _ = (#alignment WSABUF)
    peek p = do
        wbLen <- (#peek WSABUF, len) p
        wbBuf <- (#peek WSABUF, buf) p
        return $! WSABUF{..}
    poke p WSABUF{..} = do
        (#poke WSABUF, len) p wbLen
        (#poke WSABUF, buf) p wbBuf

-- | Allocate a temporary array of @WSABUF@ structures, which are used with
-- e.g. @WSARecv@ and @WSASend@.
withWSABUFs :: [(Ptr a, Int)] -> (LPWSABUF -> DWORD -> IO r) -> IO r
withWSABUFs xs body =
    withArrayLen (map toWSABUF xs) $ \bufCount bufPtr ->
    body bufPtr (fromIntegral bufCount)
  where
    toWSABUF (ptr, len) = WSABUF{ wbLen = fromIntegral len, wbBuf = castPtr ptr }

type LPWSAOVERLAPPED = LPOVERLAPPED

type LPWSAOVERLAPPED_COMPLETION_ROUTINE =
    FunPtr WSAOVERLAPPED_COMPLETION_ROUTINE

type WSAOVERLAPPED_COMPLETION_ROUTINE
    = DWORD           -- ^ dwError
   -> DWORD           -- ^ cbTransferred
   -> LPWSAOVERLAPPED -- ^ lpOverlapped
   -> DWORD           -- ^ dwFlags
   -> IO ()
