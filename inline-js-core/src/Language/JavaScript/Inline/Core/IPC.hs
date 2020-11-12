module Language.JavaScript.Inline.Core.IPC where

import Control.Concurrent
import Control.Exception
import Data.Binary.Get
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import qualified Data.ByteString.Lazy as LBS
import Foreign
import GHC.IO (catchAny)
import Language.JavaScript.Inline.Core.Utils
import System.IO
import System.IO.Unsafe

type Msg = LBS.ByteString

-- | An 'IPC' represents an opaque bi-directional message port. There are
-- middleware functions in this module which modify 'IPC' fields.
data IPC = IPC
  { -- | Send a 'Msg'.
    send :: Msg -> IO (),
    -- | Receive a 'Msg'. Should block when there's no incoming 'Msg' yet, and
    -- throw when the 'IPC' is closed.
    recv :: IO Msg,
    -- | Callback for each incoming 'Msg'.
    onRecv :: Msg -> IO (),
    -- | The 'Msg' to notify the remote device to shutdown. No more 'Msg's may
    -- be sent after this is sent, but more incoming 'Msg's are still permitted
    -- (until 'recv' throws). This allows graceful shutdown behavior.
    closeMsg :: Msg,
    -- | The callback to be called when 'recv' throws, which indicates the
    -- remote device has closed. Will only be called once.
    postClose :: IO ()
  }

instance Show IPC where
  show IPC {} = "IPC"

-- | Given the 'Handle's for send/recv, this function creates the 'send' /
-- 'recv' fields of an 'IPC' value.
--
-- The protocol for preserving message boundaries is simple: first comes the
-- message header, which is just a little-endian 64-bit unsigned integer,
-- representing the message byte length (sans header). Then follows the actual
-- message.

-- The 'Handle' for send is flushed after each 'send' call to allow the remote
-- device to get the 'Msg' immediately.
ipcFromHandles :: Handle -> Handle -> IPC -> IPC
ipcFromHandles h_send h_recv ipc =
  ipc
    { send = \msg -> do
        BS.hPut h_send $
          LBS.toStrict $
            toLazyByteString $
              storablePut (LBS.length msg)
                <> lazyByteString msg
        hFlush h_send,
      recv = do
        len <- runGet storableGet <$> hGetExact h_recv 8
        hGetExact h_recv $ fromIntegral (len :: Word64)
    }

-- | This function forks the recv thread. In the result 'IPC' value, only the
-- 'send' / 'closeMsg' fields remain valid and can be used by the user.
--
-- The recv thread repeatedly fetches incoming 'Msg's and invokes the 'onRecv'
-- callback on them. When an exception is raised, it invokes the 'postClose'
-- callback and exits. Since 'onRecv' is run in the recv thread, the user should
-- ensure it doesn't throw and doesn't take too long to complete, otherwise
-- it'll block later incoming messages.
ipcFork :: IPC -> IO IPC
ipcFork ipc = do
  post_close_thunk <- unsafeInterleaveIO $ postClose ipc
  let post_close = evaluate post_close_thunk
  let io_recv_loop = do
        msg <- recv ipc
        onRecv ipc msg
        io_recv_loop
      io_recv = catchAny io_recv_loop $ const post_close
  _ <- forkIO io_recv
  pure
    ipc
      { recv = error "fork: recv",
        onRecv = error "fork: onRecv",
        postClose = error "fork: postClose"
      }
